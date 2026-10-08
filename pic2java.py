#!/usr/bin/env python3
"""Generate Java classes from COBOL PIC record layouts.

Reads one or more COBOL record descriptions (01-level groups of PIC fields)
and writes one Java source file per record.  Each class can parse the
fixed-width string described by the layout and format itself back into it.

Usage:
    pic2java.py [-o OUTPUT_DIR] [-p PACKAGE] [FILE ...]

With no FILE (or FILE of '-'), the layout is read from standard input.

Only DISPLAY usage fields are supported, since the whole record is passed
around as characters.  Nested groups are flattened into their elementary
fields.  OCCURS, REDEFINES and binary/packed usages are rejected.

The generated code depends only on the Java 11 standard library.
"""

from __future__ import annotations

import argparse
import html
import os
import re
import sys
from dataclasses import dataclass, field
from typing import Dict, List, Optional

MAX_TEXT_LENGTH = 255  # longer fields of any type become CharBuffers
MAX_INT_DIGITS = 8
MAX_LONG_DIGITS = 18

JAVA_KEYWORDS = frozenset("""
    abstract assert boolean break byte case catch char class const continue
    default do double else enum extends final finally float for goto if
    implements import instanceof int interface long native new package private
    protected public return short static strictfp super switch synchronized
    this throw throws transient try void volatile while true false null var
    record yield
""".split())

USAGE_UNSUPPORTED = re.compile(
    r"^(COMP(-\d)?|COMPUTATIONAL(-\d)?|BINARY|PACKED-DECIMAL|POINTER|INDEX)$")


class PicError(Exception):
    def __init__(self, source: str, line: int, message: str):
        super().__init__(f"{source}:{line}: {message}")


@dataclass
class Item:
    """One data description entry from the COBOL source."""
    level: int
    name: str  # upper case, hyphens replaced with underscores
    pic: Optional[str]
    sign_leading: Optional[bool]  # None means "use the default"
    line: int
    text: str  # the entry as written, whitespace collapsed


@dataclass
class Record:
    name: str
    source: str
    line: int
    items: List[Item] = field(default_factory=list)

    def signature(self):
        return [(i.level, i.name, i.pic, i.sign_leading) for i in self.items]


@dataclass
class JavaField:
    kind: str  # text, int, long, biginteger, decimal, date, datetime, buffer, filler
    name: str  # Java field name
    cobol_name: str
    cobol_text: str
    start: int
    length: int
    digits: int = 0
    scale: int = 0
    signed: bool = False
    sign_leading: bool = False
    doc: List[str] = field(default_factory=list)

    @property
    def end(self):
        return self.start + self.length

    @property
    def java_type(self):
        return {
            "text": "String",
            "int": "Integer",
            "long": "Long",
            "biginteger": "BigInteger",
            "decimal": "BigDecimal",
            "date": "LocalDate",
            "datetime": "LocalDateTime",
            "buffer": "CharBuffer",
        }[self.kind]

    @property
    def getter(self):
        return "get" + self.name[0].upper() + self.name[1:]


# ---------------------------------------------------------------------------
# Parsing COBOL source
# ---------------------------------------------------------------------------

def strip_comments(text: str) -> str:
    """Blank out comment lines and inline comments, keeping line numbers."""
    lines = []
    for line in text.splitlines():
        if line.lstrip().startswith("*"):
            lines.append("")
        else:
            lines.append(line.split("*>", 1)[0])
    return "\n".join(lines)


def normalize_name(name: str) -> str:
    return name.upper().replace("-", "_")


def parse_records(text: str, source: str) -> List[Record]:
    text = strip_comments(text)
    records: List[Record] = []
    pos = 0
    # An entry ends at a period followed by whitespace; periods inside
    # pictures like ZZ9.99 are followed by more picture characters.
    for match in re.finditer(r"\S.*?\.(?=\s|$)", text, re.S):
        pos = match.end()
        line = text.count("\n", 0, match.start()) + 1
        item = parse_entry(match.group()[:-1], source, line)
        if item is None:
            continue
        if item.level == 1:
            if item.pic is not None:
                raise PicError(source, line, f"01-level {item.name} has a PIC clause; "
                                             "expected a group of fields")
            records.append(Record(item.name, source, line))
        elif not records:
            raise PicError(source, line, f"{item.name} appears before any 01-level record")
        else:
            records[-1].items.append(item)
    rest = text[pos:].strip()
    if rest:
        line = text.count("\n", 0, text.index(rest, pos)) + 1
        raise PicError(source, line, f"entry is missing its terminating period: {rest[:40]!r}")
    return records


def parse_entry(sentence: str, source: str, line: int) -> Optional[Item]:
    tokens = sentence.split()
    text = " ".join(tokens) + "."

    def fail(message):
        raise PicError(source, line, f"{message} in: {text}")

    if not tokens[0].isdigit():
        fail("expected a level number")
    level = int(tokens[0])
    if level == 88:
        return None  # condition names describe values, not storage
    if level == 77:
        print(f"warning: {source}:{line}: skipping 77-level item {tokens[1:2]}", file=sys.stderr)
        return None
    if level == 66:
        fail("RENAMES (66-level) entries are not supported")
    if not 1 <= level <= 49:
        fail(f"invalid level number {level}")

    i = 1
    name = "FILLER"
    if i < len(tokens) and tokens[i].upper() not in ("PIC", "PICTURE"):
        name = normalize_name(tokens[i])
        i += 1
    if not re.fullmatch(r"[A-Z0-9][A-Z0-9_]*", name):
        fail(f"invalid data name {name!r}")

    pic = None
    sign_leading = None

    def next_token(skip=()):
        nonlocal i
        while i < len(tokens) and tokens[i].upper() in skip:
            i += 1
        if i >= len(tokens):
            fail("unexpected end of entry")
        i += 1
        return tokens[i - 1].upper()

    while i < len(tokens):
        word = next_token()
        if word in ("PIC", "PICTURE"):
            pic = next_token(skip=("IS",))
        elif word == "USAGE":
            usage = next_token(skip=("IS",))
            if usage != "DISPLAY":
                fail(f"USAGE {usage} is not supported; only DISPLAY fields can be read from text")
        elif word == "DISPLAY":
            pass
        elif USAGE_UNSUPPORTED.match(word):
            fail(f"USAGE {word} is not supported; only DISPLAY fields can be read from text")
        elif word in ("SIGN", "LEADING", "TRAILING"):
            if word == "SIGN":
                word = next_token(skip=("IS",))
            if word not in ("LEADING", "TRAILING"):
                fail("expected LEADING or TRAILING after SIGN")
            sign_leading = word == "LEADING"
            if i < len(tokens) and tokens[i].upper() == "SEPARATE":
                fail("SIGN ... SEPARATE is not supported")
        elif word in ("VALUE", "VALUES"):
            break  # initial values don't affect the layout
        elif word in ("OCCURS", "REDEFINES"):
            fail(f"{word} is not supported")
        else:
            fail(f"unsupported clause {word}")

    return Item(level, name, pic, sign_leading, line, text)


# ---------------------------------------------------------------------------
# Mapping PIC clauses to Java types
# ---------------------------------------------------------------------------

@dataclass
class Picture:
    category: str  # text, numeric, other
    length: int
    digits: int = 0
    scale: int = 0
    signed: bool = False


def expand_picture(pic: str) -> str:
    """Expand repeat counts, e.g. 9(3)V99 -> 999V99."""
    out = []
    for char, count in re.findall(r"(.)(?:\((\d+)\))?", pic):
        if char in "()":
            raise ValueError(f"malformed PIC {pic}")
        out.append(char * int(count or 1))
    return "".join(out)


def classify_picture(pic: str) -> Picture:
    expanded = expand_picture(pic)
    if re.fullmatch(r"[XA]+", expanded):
        return Picture("text", len(expanded))
    numeric = re.fullmatch(r"(S?)(9*)(?:V(9*))?", expanded)
    if numeric and (numeric.group(2) or numeric.group(3)):
        integer, fraction = numeric.group(2), numeric.group(3) or ""
        digits = len(integer) + len(fraction)
        return Picture("numeric", digits, digits, len(fraction), bool(numeric.group(1)))
    # Edited pictures (Z, commas, periods, CR/DB...) are display-only text.
    # S, V and P don't occupy a character position.
    return Picture("other", len(re.sub(r"[SVP]", "", expanded)))


def camel_case(name: str, capitalize: bool = False) -> str:
    parts = [p for p in name.split("_") if p]
    words = [p.lower() for p in parts]
    result = "".join(w[0].upper() + w[1:] for w in words)
    if not capitalize:
        result = words[0] + result[len(words[0]):]
    if result[0].isdigit():
        result = ("Record" if capitalize else "field") + result[0].upper() + result[1:]
    if result in JAVA_KEYWORDS:
        result += "_"
    return result


def is_date_digits(pic: Picture) -> bool:
    return pic.category == "numeric" and pic.digits == 8 and pic.scale == 0 and not pic.signed


def build_fields(record: Record, default_leading: bool) -> List[JavaField]:
    items = [item for item in record.items if item.pic is not None]
    if not items:
        raise PicError(record.source, record.line, f"{record.name} has no PIC fields")

    def picture(item):
        try:
            return classify_picture(item.pic)
        except ValueError as e:
            raise PicError(record.source, item.line, str(e))

    fields: List[JavaField] = []
    pos = 0
    i = 0
    while i < len(items):
        item = items[i]
        pic = picture(item)
        sources = [item.text]
        notes = []
        f = JavaField("text", camel_case(item.name), item.name, item.text, pos, pic.length)

        if item.name == "FILLER":
            f.kind = "filler"
        elif pic.length > MAX_TEXT_LENGTH:
            f.kind = "buffer"
        elif pic.category == "text":
            f.kind = "text"
        elif pic.category == "other":
            f.kind = "text"
            notes.append("Edited PIC is not interpreted; the raw characters are kept as text.")
            print(f"warning: {record.source}:{item.line}: {item.name} PIC {item.pic} "
                  "is not a plain alphanumeric or numeric picture; treating it as text",
                  file=sys.stderr)
        elif item.name.endswith("DATE") and is_date_digits(pic):
            prefix = item.name[:-len("DATE")]
            following = items[i + 1] if i + 1 < len(items) else None
            if (following is not None and following.name == prefix + "TIME"
                    and is_date_digits(picture(following))):
                prefix = prefix.rstrip("_")
                f.kind = "datetime"
                f.name = camel_case(prefix) if prefix else "dateTime"
                f.length = 16
                f.cobol_text += " " + following.text
                sources.append(following.text)
                notes.append("Combined into a single date and time (yyyyMMdd + HHmmss + hundredths).")
                i += 1
            else:
                f.kind = "date"
                notes.append("Interpreted as a yyyyMMdd date.")
        else:
            f.digits, f.scale, f.signed = pic.digits, pic.scale, pic.signed
            if pic.scale:
                f.kind = "decimal"
            elif pic.digits <= MAX_INT_DIGITS:
                f.kind = "int"
            elif pic.digits <= MAX_LONG_DIGITS:
                f.kind = "long"
            else:
                f.kind = "biginteger"
            if pic.signed:
                f.sign_leading = default_leading if item.sign_leading is None else item.sign_leading

        if item.sign_leading is not None and not f.signed:
            print(f"warning: {record.source}:{item.line}: ignoring SIGN clause on "
                  f"{item.name}, which is not a signed number", file=sys.stderr)

        code = [f"<code>{html.escape(text)}</code>" for text in sources]
        f.doc = [f"Positions {f.start + 1}-{f.end}: {code[0]}"] + code[1:] + notes
        fields.append(f)
        pos += f.length
        i += 1

    seen: Dict[str, JavaField] = {}
    for f in fields:
        if f.kind == "filler":
            continue
        if f.name in seen:
            raise PicError(record.source, record.line,
                           f"{record.name}: {seen[f.name].cobol_name} and {f.cobol_name} "
                           f"both map to the Java name {f.name}")
        seen[f.name] = f
    return fields


# ---------------------------------------------------------------------------
# Java generation
# ---------------------------------------------------------------------------

def java_string(s: str) -> str:
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'


def java_bool(b: bool) -> str:
    return "true" if b else "false"


def parse_expression(f: JavaField) -> str:
    name = java_string(f.cobol_name)
    if f.kind == "text":
        return f"text(chars, {f.start}, {f.end})"
    if f.kind == "buffer":
        return f"CharBuffer.wrap(chars.subSequence({f.start}, {f.end}).toString())"
    if f.kind == "date":
        return f"date(chars, {f.start}, {f.end}, {name})"
    if f.kind == "datetime":
        return f"dateTime(chars, {f.start}, {f.end}, {name})"
    zoned = f"zoned(chars, {f.start}, {f.end}, {java_bool(f.signed)}, {name})"
    return {
        "int": f"Integer.parseInt({zoned})",
        "long": f"Long.parseLong({zoned})",
        "biginteger": f"new BigInteger({zoned})",
        "decimal": f"new BigDecimal(new BigInteger({zoned}), {f.scale})",
    }[f.kind]


def parse_null(f: JavaField, prs: str) -> str:
    if f.kind in ["text", "buffer", "date", "datetime", "filler"]:
        return prs
    else:
        return f"isNullOrBlank(chars, {f.start}, {f.end}) ? null : {prs}"



def check_statement(f: JavaField) -> str:
    n, q = f.name, java_string(f.name)
    signed = java_bool(f.signed)
    if f.kind == "text":
        return f"this.{n} = checkText({q}, {n}, {f.length});"
    if f.kind == "buffer":
        return f"this.{n} = checkBuffer({q}, {n}, {f.length});"
    if f.kind == "date":
        return f"this.{n} = checkDate({q}, {n});"
    if f.kind == "datetime":
        return f"this.{n} = checkDateTime({q}, {n});"
    if f.kind in ("int", "long"):
        return (f"checkNumber({q}, BigInteger.valueOf({n}), {f.digits}, {signed});\n"
                f"this.{n} = {n};")
    if f.kind == "biginteger":
        return f"this.{n} = checkNumber({q}, {n}, {f.digits}, {signed});"
    return f"this.{n} = checkDecimal({q}, {n}, {f.digits}, {f.scale}, {signed});"


def check_null(f: JavaField, ck: str) -> str:
    n = f.name
    if f.kind in ["text", "buffer", "date", "datetime", "filler"]:
        return ck
    else:
        return (f"if ({n} == null) {{\n"
                f"    this.{n} = null;\n"
                "}\n"
                "else {\n"
                f"    {ck}\n"
                "}");

def format_statement(f: JavaField) -> str:
    n = f.name
    leading = java_bool(f.sign_leading)
    if f.kind == "filler":
        return f'out.append(" ".repeat({f.length}));'
    if f.kind == "text":
        return f"putText(out, {n}, {f.length});"
    if f.kind == "buffer":
        return f"out.append({n});"
    if f.kind == "date":
        return f'out.append({n} == null ? "00000000" : DATE_FORMAT.format({n}));'
    if f.kind == "datetime":
        return f"putDateTime(out, {n});"
    if f.kind in ("int", "long"):
        return f"putZoned(out, BigInteger.valueOf({n}), {f.digits}, {leading});"
    if f.kind == "biginteger":
        return f"putZoned(out, {n}, {f.digits}, {leading});"
    return f"putZoned(out, {n}.unscaledValue(), {f.digits}, {leading});"

def format_null(f: JavaField, fmt: str) -> str:
    n = f.name
    if f.kind in ["text", "buffer", "date", "datetime", "filler"]:
        return fmt
    else:
        return (f"if (this.{n} == null) {{\n"
                f"    putText(out, \"\", {f.length});\n"
                "}\n"
                "else {\n"
                f"    {fmt}\n"
                "}\n")


def builder_default(f: JavaField) -> str:
    return {
        "text": ' = ""',
        "biginteger": " = BigInteger.ZERO",
        "decimal": " = BigDecimal.ZERO",
        "buffer": ' = CharBuffer.wrap("")',
    }.get(f.kind, "")


HELPERS = {
    "always": """
    /** Checks if a string is null or a char subsequence is all-spaces */
    private static boolean isNullOrBlank(CharSequence chars, int start, int end) {
        if (chars == null) {
            return true;
        }
        else for (int i = start; i < end; i++) {
            if (chars.charAt(i) != ' ') { return false; }
        }
        return true;
    }

    /** Pads a character sequence to the desired length with spaces */
    private static CharSequence padToLength(String name, CharSequence value, int length) {
        Objects.requireNonNull(value, name);
        if (value.length() > length) {
            throw new IllegalArgumentException(name + " is longer than " + length + " characters");
        }
        StringBuilder padded = new StringBuilder(length).append(value);
        padded.append(" ".repeat(length - padded.length()));
        return padded;
    }
""",
    "text": """
    private static String text(CharSequence chars, int start, int end) {
        int last = end;
        while (last > start && chars.charAt(last - 1) == ' ') {
            last--;
        }
        return chars.subSequence(start, last).toString();
    }

    private static String checkText(String name, String value, int length) {
        Objects.requireNonNull(value, name);
        int last = value.length();
        while (last > 0 && value.charAt(last - 1) == ' ') {
            last--;
        }
        if (last > length) {
            throw new IllegalArgumentException(name + " is longer than " + length + " characters");
        }
        return value.substring(0, last);
    }

    private static void putText(StringBuilder out, String value, int length) {
        out.append(value);
        if (value.length() < length) {
            out.append(" ".repeat(value.length() - length));
        }
    }
""",
    "numeric": """
    /*
     * Signed fields carry their sign "overpunched" on the first or last digit.
     * Plain digits are treated as positive. The ASCII (Micro Focus style)
     * negative characters are accepted when reading, but negative numbers
     * are always written with the EBCDIC-derived characters.
     */
    private static final String POSITIVE_OVERPUNCH = "{ABCDEFGHI";
    private static final String NEGATIVE_OVERPUNCH = "}JKLMNOPQR";
    private static final String NEGATIVE_ASCII_OVERPUNCH = "pqrstuvwxy";

    private static String zoned(CharSequence chars, int start, int end, boolean signed, String name) {
        StringBuilder digits = new StringBuilder(end - start + 1);
        boolean negative = false;
        boolean sawSign = false;
        for (int i = start; i < end; i++) {
            char c = chars.charAt(i);
            if (c >= '0' && c <= '9') {
                digits.append(c);
                continue;
            }
            int digit = -1;
            if (signed && !sawSign && (i == start || i == end - 1)) {
                digit = POSITIVE_OVERPUNCH.indexOf(c);
                if (digit < 0) {
                    digit = NEGATIVE_OVERPUNCH.indexOf(c);
                    if (digit < 0) {
                        digit = NEGATIVE_ASCII_OVERPUNCH.indexOf(c);
                    }
                    negative = digit >= 0;
                }
            }
            if (digit < 0) {
                throw new IllegalArgumentException(
                        name + " is not a valid number: \\"" + chars.subSequence(start, end) + "\\"");
            }
            sawSign = true;
            digits.append((char) ('0' + digit));
        }
        return negative ? "-" + digits : digits.toString();
    }

    private static BigInteger checkNumber(String name, BigInteger value, int digits, boolean signed) {
        Objects.requireNonNull(value, name);
        if (!signed && value.signum() < 0) {
            throw new IllegalArgumentException(name + " cannot be negative: " + value);
        }
        if (value.abs().toString().length() > digits) {
            throw new IllegalArgumentException(name + " has more than " + digits + " digits: " + value);
        }
        return value;
    }

    private static void putZoned(StringBuilder out, BigInteger value, int digits, boolean leadingSign) {
        String magnitude = value.abs().toString();
        int start = out.length();
        for (int i = magnitude.length(); i < digits; i++) {
            out.append('0');
        }
        out.append(magnitude);
        if (value.signum() < 0) {
            int signAt = leadingSign ? start : out.length() - 1;
            out.setCharAt(signAt, NEGATIVE_OVERPUNCH.charAt(out.charAt(signAt) - '0'));
        }
    }
""",
    "decimal": """
    private static BigDecimal checkDecimal(String name, BigDecimal value, int digits, int scale, boolean signed) {
        Objects.requireNonNull(value, name);
        BigDecimal scaled;
        try {
            scaled = value.setScale(scale, RoundingMode.UNNECESSARY);
        } catch (ArithmeticException e) {
            throw new IllegalArgumentException(name + " has more than " + scale + " decimal places: " + value, e);
        }
        if (!signed && scaled.signum() < 0) {
            throw new IllegalArgumentException(name + " cannot be negative: " + value);
        }
        if (scaled.unscaledValue().abs().toString().length() > digits) {
            throw new IllegalArgumentException(
                    name + " has more than " + (digits - scale) + " digits before the decimal point: " + value);
        }
        return scaled;
    }
""",
    "anydate": """
    /** All zeros or all spaces means "no date". */
    private static boolean isEmptyDate(CharSequence chars, int start, int end) {
        for (int i = start; i < end; i++) {
            char c = chars.charAt(i);
            if (c != '0' && c != ' ') {
                return false;
            }
        }
        return true;
    }

    private static LocalDate checkDate(String name, LocalDate value) {
        if (value != null && (value.getYear() < 0 || value.getYear() > 9999)) {
            throw new IllegalArgumentException(name + " must have a four-digit year: " + value);
        }
        return value;
    }
""",
    "date": """
    private static final DateTimeFormatter DATE_FORMAT =
            DateTimeFormatter.ofPattern("uuuuMMdd").withResolverStyle(ResolverStyle.STRICT);

    private static LocalDate date(CharSequence chars, int start, int end, String name) {
        if (isEmptyDate(chars, start, end)) {
            return null;
        }
        String value = chars.subSequence(start, end).toString();
        try {
            return LocalDate.parse(value, DATE_FORMAT);
        } catch (DateTimeParseException e) {
            throw new IllegalArgumentException(name + " is not a valid yyyyMMdd date: \\"" + value + "\\"", e);
        }
    }
""",
    "datetime": """
    /** The 16-character date and time, with a "0" appended to make the hundredths into milliseconds. */
    private static final DateTimeFormatter DATE_TIME_FORMAT =
            DateTimeFormatter.ofPattern("uuuuMMddHHmmssSSS").withResolverStyle(ResolverStyle.STRICT);

    private static LocalDateTime dateTime(CharSequence chars, int start, int end, String name) {
        if (isEmptyDate(chars, start, end)) {
            return null;
        }
        String value = chars.subSequence(start, end).toString();
        try {
            return LocalDateTime.parse(value + "0", DATE_TIME_FORMAT);
        } catch (DateTimeParseException e) {
            throw new IllegalArgumentException(
                    name + " is not a valid yyyyMMdd date and HHmmssSS time: \\"" + value + "\\"", e);
        }
    }

    /** Only hundredths of a second fit, so anything finer is truncated. */
    private static LocalDateTime checkDateTime(String name, LocalDateTime value) {
        if (value == null) {
            return null;
        }
        checkDate(name, value.toLocalDate());
        return value.withNano(value.getNano() / 10_000_000 * 10_000_000);
    }

    private static void putDateTime(StringBuilder out, LocalDateTime value) {
        if (value == null) {
            out.append("0000000000000000");
        } else {
            out.append(DATE_TIME_FORMAT.format(value), 0, 16);
        }
    }
""",
    "buffer": """
    /** Copies the value into a read-only buffer padded with spaces to the full field length. */
    private static CharBuffer checkBuffer(String name, CharBuffer value, int length) {
        Objects.requireNonNull(value, name);
        if (value.remaining() > length) {
            throw new IllegalArgumentException(name + " is longer than " + length + " characters");
        }
        StringBuilder padded = new StringBuilder(length).append(value);
        padded.append(" ".repeat(length - padded.length()));
        return CharBuffer.wrap(padded.toString());
    }
""",
}


def indent(text: str, spaces: int) -> str:
    pad = " " * spaces
    return "\n".join(pad + line if line else line for line in text.split("\n"))


def generate_class(record: Record, fields: List[JavaField], debug: bool, package: Optional[str]) -> str:
    cls = camel_case(record.name, capitalize=True)
    data = [f for f in fields if f.kind != "filler"]
    kinds = {f.kind for f in fields}
    total = sum(f.length for f in fields)

    helpers = ["always"]
    if "text" in kinds:
        helpers.append("text")
    if kinds & {"int", "long", "biginteger", "decimal"}:
        helpers.append("numeric")
    if "decimal" in kinds:
        helpers.append("decimal")
    if kinds & {"date", "datetime"}:
        helpers.append("anydate")
    helpers += [k for k in ("date", "datetime", "buffer") if k in kinds]

    imports = []
    if "decimal" in kinds:
        imports += ["java.math.BigDecimal", "java.math.RoundingMode"]
    if kinds & {"int", "long", "biginteger", "decimal"}:
        imports.append("java.math.BigInteger")
    if "buffer" in kinds:
        imports.append("java.nio.CharBuffer")
    if "date" in kinds or "datetime" in kinds:
        imports.append("java.time.LocalDate")
    if "datetime" in kinds:
        imports.append("java.time.LocalDateTime")
    if kinds & {"date", "datetime"}:
        imports += ["java.time.format.DateTimeFormatter", "java.time.format.DateTimeParseException",
                    "java.time.format.ResolverStyle"]
    imports += ["java.util.Arrays", "java.util.Objects"]

    source_lines = [f"01 {record.name}."] + ["    " + item.text for item in record.items]

    out: List[str] = []
    w = out.append
    if package:
        w(f"package {package};\n")
    for imp in sorted(imports):
        w(f"import {imp};")
    w("")
    w("/**")
    w(f" * The {record.name} record, {total} characters long.")
    w(" *")
    w(" * <p>Generated by pic2java.py from:")
    w(" * <pre>")
    for line in source_lines:
        w(" * " + html.escape(line).replace("*/", "*&#47;"))
    w(" * </pre>")
    w(" */")
    w(f"public class {cls} {{")
    w("")
    w("    /** The length of the fixed-width text form of this record. */")
    w(f"    public static final int LENGTH = {total};")
    for f in data:
        w("")
        w("    /**")
        for n, line in enumerate(f.doc):
            w(("     * " if n == 0 else "     * <br>") + line)
        w("     */")
        w(f"    private final {f.java_type} {f.name};")
    w("")

    # Constructor from text
    w("    /**")
    w(f"     * Parses the {total}-character text form of this record.")
    w("     *")
    w("     * @throws IllegalArgumentException if the text is the wrong length or a field can't be parsed")
    w("     */")
    w(f"    protected {cls}(CharSequence chars) {{")
    w('        Objects.requireNonNull(chars, "chars");')
    w("        if (chars.length() != LENGTH) {")
    w("            throw new IllegalArgumentException(")
    w(f'                    "{cls} must be " + LENGTH + " characters, but was " + chars.length());')
    w("        }")
    for f in data:
        w(f"        this.{f.name} = {parse_null(f, parse_expression(f))};")
        if debug:
            w(indent(f"System.err.println(\"parse: {f.name:20} ({f.cobol_text:50}) < (\" + chars.subSequence({f.start}, {f.end}) + \")\");", 8))
    w("    }")
    w("")

    # Constructor from fields
    params = ",\n".join(f"            {f.java_type} {f.name}" for f in data)
    w("    /**")
    w("     * Creates a record from its field values.")
    w("     *")
    w("     * @throws IllegalArgumentException if a value doesn't fit in its field")
    w("     */")
    w(f"    protected {cls}(\n{params}) {{")
    for f in data:
        w(indent(check_null(f, check_statement(f)), 8))
        if debug:
            w(indent(f"System.err.println(\"check: {f.name:20} ({f.cobol_text:50})\");", 8))
    w("    }")
    w("")

    w("    /** Parses the text form of this record. */")
    w(f"    public static {cls} fromChars(CharSequence chars) {{")
    w(f"        return new {cls}(padToLength(\"chars\", chars, LENGTH));")
    w("    }")
    w("")
    w("    public static Builder builder() {")
    w("        return new Builder();")
    w("    }")

    for f in data:
        w("")
        w(f"    public {f.java_type} {f.getter}() {{")
        if f.kind == "buffer":
            w(f"        return {f.name}.duplicate();")
        else:
            w(f"        return {f.name};")
        w("    }")
    w("")

    w("    private Object[] fields() {")
    w("        return new Object[] {")
    w(",\n".join(f"            {f.name}" for f in data))
    w("        };")
    w("    }")
    w("")
    w("    @Override")
    w("    public boolean equals(Object other) {")
    w("        if (this == other) {")
    w("            return true;")
    w("        }")
    w("        if (other == null || getClass() != other.getClass()) {")
    w("            return false;")
    w("        }")
    w(f"        return Arrays.deepEquals(fields(), (({cls}) other).fields());")
    w("    }")
    w("")
    w("    @Override")
    w("    public int hashCode() {")
    w("        return Arrays.deepHashCode(fields());")
    w("    }")
    w("")
    w("    /** Formats this record as its fixed-width text form. */")
    w("    @Override")
    w("    public String toString() {")
    w("        StringBuilder out = new StringBuilder(LENGTH);")
    for f in fields:
        w(indent(format_null(f, format_statement(f)), 8))
    w("        return out.toString();")
    w("    }")

    for h in helpers:
        w(HELPERS[h].rstrip("\n"))
    w("")

    w(f"    /** Builds a {cls}. Fields that aren't set are blank, zero, or null for dates. */")
    w("    public static class Builder {")
    for f in data:
        w(f"        private {f.java_type} {f.name}{builder_default(f)};")
    w("")
    w("        public Builder() {")
    w("        }")
    for f in data:
        w("")
        w(f"        public Builder {f.name}({f.java_type} {f.name}) {{")
        w(f"            this.{f.name} = {f.name};")
        w("            return this;")
        w("        }")
    w("")
    w(f"        public {cls} build() {{")
    args = ",\n".join(f"                    {f.name}" for f in data)
    w(f"            return new {cls}(\n{args});")
    w("        }")
    w("    }")
    w("}")
    return "\n".join(out) + "\n"


# ---------------------------------------------------------------------------
# Command line
# ---------------------------------------------------------------------------

def main(argv=None) -> int:
    parser = argparse.ArgumentParser(
        description="Generate Java classes from COBOL PIC record layouts.")
    parser.add_argument("inputs", nargs="*", metavar="FILE",
                        help="files containing 01-level record layouts (default: standard input)")
    parser.add_argument("-o", "--output-dir", default=".",
                        help="directory to write the .java files to (default: current directory)")
    parser.add_argument("-p", "--package", help="Java package for the generated classes")
    parser.add_argument("-d", "--debug", action="store_true",
                        help="add debug output (via System.err) in Java file")
    parser.add_argument("--sign-position", choices=("trailing", "leading"), default="trailing",
                        help="which digit carries the sign when writing negative numbers to "
                             "signed (S9) fields without a SIGN clause (default: trailing, "
                             "the COBOL default)")
    args = parser.parse_args(argv)

    if args.package and not re.fullmatch(r"[A-Za-z_]\w+(\.[A-Za-z_]\w+)*", args.package):
        parser.error(f"invalid package name {args.package!r}")

    debug = bool(args.debug)

    try:
        records: List[Record] = []
        for path in args.inputs or ["-"]:
            if path == "-":
                records += parse_records(sys.stdin.read(), "<stdin>")
            else:
                with open(path, encoding="utf-8") as f:
                    records += parse_records(f.read(), path)

        # The same layout often appears in several documents; generate it once.
        unique: Dict[str, Record] = {}
        for record in records:
            cls = camel_case(record.name, capitalize=True)
            previous = unique.get(cls)
            if previous is None:
                unique[cls] = record
            elif previous.signature() != record.signature():
                raise PicError(record.source, record.line,
                               f"{record.name} conflicts with the different layout at "
                               f"{previous.source}:{previous.line}")

        outputs = []
        for cls, record in unique.items():
            fields = build_fields(record, args.sign_position == "leading")
            outputs.append((cls, fields, generate_class(record, fields, debug, args.package)))
    except PicError as e:
        print(f"error: {e}", file=sys.stderr)
        return 1
    except OSError as e:
        print(f"error: {e}", file=sys.stderr)
        return 1

    if not outputs:
        print("error: no 01-level records found", file=sys.stderr)
        return 1

    os.makedirs(args.output_dir, exist_ok=True)
    for cls, fields, code in outputs:
        path = os.path.join(args.output_dir, cls + ".java")
        with open(path, "w", encoding="utf-8") as f:
            f.write(code)
        length = sum(f.length for f in fields)
        count = sum(1 for f in fields if f.kind != "filler")
        print(f"wrote {path} ({count} fields, {length} characters)", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
