use std::cmp::min;
use std::str::from_utf8_unchecked;
use thiserror::Error;

#[derive(Error, Debug, PartialEq, Eq, Clone)]
pub enum Error {
    #[error("expected a \" but found: {0:?}")]
    StartingSequenceNotFound(Option<u8>),

    /// An unterminated string literal was found
    ///
    /// Apart from forgetting the ending `"`, terminating a string within a
    /// Unicode escape sequence or having a line break in the string also
    /// causes this error.
    #[error("string is unterminated")]
    Unterminated,

    /// An unknown character in a string literal was found
    ///
    /// This occurs when an invalid source character is found in a string
    /// literal, such as ASCII control characters.
    #[error("unexpected character in string: {0:?}")]
    UnknownCharacterInString(char),

    /// An unknown escape sequence in a string literal was found
    ///
    /// Only a limited set of escape sequences are supported, this is emitted
    /// when e.g. `"\l"` is parsed.
    #[error("unknown escape sequence in string: {0:?}")]
    UnknownEscapeSequence(String),

    /// An invalid unicode escape sequence in a string literal was found, and
    /// an error message is provided.
    ///
    /// This began with `"\u"` being parsed and then something going wrong.
    #[error("invalid unicode escape sequence in string. {0:?}")]
    InvalidUnicodeEscapeSequence(String),
}

pub struct Consumed {
    /// Number of line breaks consumed.
    pub line_breaks: usize,
    /// Number of characters consumed without hitting a further line break.
    pub chars_without_further_line_break: usize,
}

impl Consumed {
    fn no_line_break(chars: usize) -> Consumed {
        Consumed {
            line_breaks: 0,
            chars_without_further_line_break: chars,
        }
    }

    fn count_line_break(&mut self) {
        self.line_breaks += 1;
        self.chars_without_further_line_break = 0;
    }

    fn count_chars(&mut self, chars: usize) {
        self.chars_without_further_line_break += chars;
    }
}

fn num_bytes_in_char(first_byte: u8) -> usize {
    if first_byte <= 0x7F {
        1
    } else if first_byte <= 0b1101_1111 {
        2
    } else if first_byte <= 0b1110_1111 {
        3
    } else {
        4
    }
}

fn is_whitespace(b: u8) -> bool {
    b == b' ' || b == b'\t'
}

struct Line {
    start: usize,
    end: usize,
    escape_chars: Vec<usize>,
}

impl Line {
    fn len(&self) -> usize {
        self.end - self.start
    }

    fn trim_front(&mut self, count: usize) {
        self.start = min(self.start + count, self.end);
    }

    fn iter<'bytes>(&self, bytes: &'bytes [u8]) -> impl Iterator<Item = u8> + 'bytes {
        bytes[self.start..self.end].iter().copied()
    }

    fn append_to(&self, bytes: &[u8], s: &mut String) {
        let mut i = self.start;
        for &escape_char_i in &self.escape_chars {
            s.push_str(unsafe { from_utf8_unchecked(&bytes[i..escape_char_i]) });
            i = escape_char_i + 1;
        }
        s.push_str(unsafe { from_utf8_unchecked(&bytes[i..self.end]) });
    }

    fn new(start: usize) -> Line {
        Line {
            start,
            end: start,
            escape_chars: vec![],
        }
    }
}

// See https://spec.graphql.org/October2021/#sec-String-Value.Block-Strings
fn parse_block_string(bytes: &[u8]) -> Result<(String, Consumed, usize), (Error, Consumed)> {
    if !matches!(bytes, [b'"', b'"', b'"', ..]) {
        return Err((
            Error::StartingSequenceNotFound(bytes.first().copied()),
            Consumed::no_line_break(0),
        ));
    }
    let mut lines: Vec<Line> = Vec::new();
    let mut i = 3;
    let mut consumed = Consumed::no_line_break(3);
    let mut cur_line = Line::new(i);
    let mut terminated = false;
    while i < bytes.len() {
        match bytes[i..] {
            [b'"', b'"', b'"', ..] => {
                cur_line.end = i;
                lines.push(cur_line);
                consumed.count_chars(3);
                i += 3;
                terminated = true;
                break;
            }
            [b'\\', b'"', b'"', b'"', ..] => {
                cur_line.escape_chars.push(i);
                consumed.count_chars(4);
                i += 4;
            }
            [b'\r', b'\n', ..] => {
                cur_line.end = i;
                lines.push(cur_line);
                consumed.count_line_break();
                i += 2;
                cur_line = Line::new(i);
            }
            [b'\n' | b'\r', ..] => {
                cur_line.end = i;
                lines.push(cur_line);
                consumed.count_line_break();
                i += 1;
                cur_line = Line::new(i);
            }
            [b, ..] => {
                consumed.count_chars(1);
                i += num_bytes_in_char(b);
            }
            _ => {
                // Should never happen since i < bytes.len()
                break;
            }
        }
    }
    if !terminated {
        return Err((Error::Unterminated, consumed));
    }

    // Figure out the common indentation in the block
    let common_indent = lines
        .iter()
        // First line is excluded
        .skip(1)
        // Don't consider whitespace only lines
        .filter_map(|line| {
            let indent = line.iter(bytes).take_while(|b| is_whitespace(*b)).count();
            (indent < line.len()).then_some(indent)
        })
        // The minimum of all indents would be the common indent
        .min();

    // Trim the common indentation from the lines (excluding the first line)
    if let Some(common_indent) = common_indent {
        for line in &mut lines[1..] {
            line.trim_front(common_indent);
        }
    }

    // Count whitespace-only lines in the beginning and end of the block and skip them
    let empty_front = lines
        .iter()
        .take_while(|line| line.iter(bytes).all(is_whitespace))
        .count();
    let empty_back = lines
        .iter()
        // Skip the lines that are already known to be empty
        .skip(empty_front)
        .rev()
        .take_while(|line| line.iter(bytes).all(is_whitespace))
        .count();

    let mut result = String::new();
    for (line_index, line) in lines[empty_front..(lines.len() - empty_back)]
        .iter()
        .enumerate()
    {
        if line_index > 0 {
            result.push('\n');
        }
        line.append_to(bytes, &mut result);
    }
    Ok((result, consumed, i))
}

// See https://spec.graphql.org/October2021/#StringValue
fn parse_single_line_string(bytes: &[u8]) -> Result<(String, Consumed, usize), (Error, Consumed)> {
    let mut result = String::new();
    // counts the unicode scalar values
    let mut chars = 1;
    let mut mark = 1;
    let mut end_reached = false;
    let mut i = 1;
    while i < bytes.len() {
        match bytes[i] {
            b'\\' => {
                let (unescaped_char, bytes_consumed) = if i + 1 < bytes.len() {
                    match bytes[i + 1] {
                        b'"' => ('\u{0022}', 1),
                        b'\\' => ('\u{005C}', 1),
                        b'/' => ('\u{002F}', 1),
                        b'b' => ('\u{0008}', 1),
                        b'f' => ('\u{000C}', 1),
                        b'n' => ('\u{000A}', 1),
                        b'r' => ('\u{000D}', 1),
                        b't' => ('\u{0009}', 1),
                        // unicode escape sequence:
                        b'u' => {
                            if i + 5 >= bytes.len() {
                                return Err((Error::Unterminated, Consumed::no_line_break(chars)));
                            }

                            // Collect the next 4 characters as hex digits
                            let hex_str = &bytes[i + 2..i + 6];
                            let hex = std::str::from_utf8(hex_str).map_err(|_| {
                                (
                                    Error::InvalidUnicodeEscapeSequence(
                                        "invalid format".to_string(),
                                    ),
                                    Consumed::no_line_break(chars),
                                )
                            })?;

                            let code_point = u32::from_str_radix(hex, 16).map_err(|_| {
                                (
                                    Error::InvalidUnicodeEscapeSequence(
                                        "invalid hex digits".to_string(),
                                    ),
                                    Consumed::no_line_break(chars),
                                )
                            })?;

                            let c = std::char::from_u32(code_point).ok_or_else(|| {
                                (
                                    Error::InvalidUnicodeEscapeSequence(format!(
                                        "0x{code_point:X} is not a valid code point"
                                    )),
                                    Consumed::no_line_break(chars),
                                )
                            })?;

                            (c, 5) // 5 bytes consumed: u, and the 4 hex digits
                        }
                        b => {
                            return Err((
                                Error::UnknownEscapeSequence(format!("\\{b}")),
                                Consumed::no_line_break(chars),
                            ))
                        }
                    }
                } else {
                    return Err((Error::Unterminated, Consumed::no_line_break(chars)));
                };
                let string_part = unsafe { from_utf8_unchecked(&bytes[mark..i]) };
                result.push_str(string_part);
                result.push(unescaped_char);
                mark = i + bytes_consumed + 1;
                i += bytes_consumed + 1;
                chars += bytes_consumed + 1;
            }
            b'"' => {
                end_reached = true;
                chars += 1;
                i += 1;
                break;
            }
            b'\n' | b'\r' => {
                return Err((Error::Unterminated, Consumed::no_line_break(chars)));
            }
            b => {
                i += num_bytes_in_char(b);
                chars += 1;
            }
        }
    }
    // we did not see a string terminator '"'
    if !end_reached {
        Err((Error::Unterminated, Consumed::no_line_break(chars)))
    // no escape sequences were found, so we can force the byte array to be a str
    } else if mark == 1 {
        Ok((
            unsafe { from_utf8_unchecked(&bytes[1..i - 1]) }.into(),
            Consumed::no_line_break(chars),
            i,
        ))
    // there are escape sequences in the string, so push the last fragment
    // without escape sequences
    } else {
        result.push_str(unsafe { from_utf8_unchecked(&bytes[mark..i - 1]) });
        Ok((result, Consumed::no_line_break(chars), i))
    }
}

pub fn parse_string(bytes: &[u8]) -> Result<(String, Consumed, usize), (Error, Consumed)> {
    match bytes {
        [b'"', b'"', b'"', ..] => parse_block_string(bytes),
        [b'"', ..] => parse_single_line_string(bytes),
        _ => Err((
            Error::StartingSequenceNotFound(bytes.first().copied()),
            Consumed::no_line_break(0),
        )),
    }
}
