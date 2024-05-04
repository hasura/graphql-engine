// use num_bigint::BigInt;

use std::fmt::Display;

use thiserror::Error;

#[derive(Debug, PartialEq, Clone)]
pub enum NumberToken {
    // Float32(f32),
    Float64(f64),
    // Int32(i32),
    Int64(i64),
}

impl Display for NumberToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumberToken::Float64(float) => float.fmt(f),
            NumberToken::Int64(i) => i.fmt(f),
        }
    }
}

#[derive(Error, Debug, PartialEq, Eq, Clone)]
pub enum Error {
    #[error("expected a digit, but found: {found:?}")]
    DigitExpected { found: Option<u8> },
    #[error("failed to parse a number: {error:?}")]
    LexicalError { error: lexical_core::Error },
    #[error("lookahead of a number cannot be a 'NameStart': {0:?}")]
    NameStart(u8),
}

pub fn parse_number(bytes: &[u8]) -> Result<(NumberToken, usize), Error> {
    let number_start = 0;
    let mut ix = 0;
    // Parse the optional negative sign
    if let Some(&b'-') = bytes.get(ix) {
        ix += 1;
    }
    // Count the number of integers present, until you hit a non-digit.
    // This helps to distinguish between integers and floats
    let integer_count = bytes[ix..]
        .iter()
        .take_while(|&&b| b.is_ascii_digit())
        .count();
    if integer_count == 0 {
        return Err(Error::DigitExpected {
            found: bytes.get(ix).copied(),
        });
    }
    let (parse_as_integer, _byte_range) = if let Some(&c) = bytes.get(ix + integer_count) {
        match c {
            // parse as float
            b'.' | b'e' | b'E' => (false, bytes),

            // parse as integer
            _ => (true, &bytes[..integer_count]),
        }
    } else {
        (true, &bytes[..integer_count])
    };

    let (token, bytes_consumed) = if parse_as_integer {
        match lexical_core::parse_partial::<i64>(&bytes[number_start..]) {
            Ok((i64, bytes_consumed)) => Ok((NumberToken::Int64(i64), bytes_consumed)),
            Err(error) => Err(Error::LexicalError { error }),
        }
    } else {
        match lexical_core::parse_partial::<f64>(&bytes[number_start..]) {
            Ok((f, bytes_consumed)) => Ok((NumberToken::Float64(f), bytes_consumed)),
            Err(error) => Err(Error::LexicalError { error }),
        }
    }?;
    if let Some(&look_ahead) = bytes.get(bytes_consumed) {
        match look_ahead {
            b'a'..=b'z' | b'A'..=b'Z' => Err(Error::NameStart(look_ahead)),
            _ => Ok((token, bytes_consumed)),
        }
    } else {
        Ok((token, bytes_consumed))
    }
}

#[cfg(test)]
mod tests {
    use super::{parse_number, Error, NumberToken};

    #[test]
    fn test_float() {
        // Float number with a single digit integral part
        let bytes = b"1.01";
        let (token, bytes_consumed) = parse_number(bytes).unwrap();
        assert_eq!(token, NumberToken::Float64(1.01));
        assert_eq!(bytes_consumed, 4);

        // Float number with multi digit integral part
        let bytes = b"123.014";
        let (token, bytes_consumed) = parse_number(bytes).unwrap();
        assert_eq!(token, NumberToken::Float64(123.014));
        assert_eq!(bytes_consumed, 7);

        // Float number with negative integral part
        let bytes = b"-123.014";
        let (token, bytes_consumed) = parse_number(bytes).unwrap();
        assert_eq!(token, NumberToken::Float64(-123.014));
        assert_eq!(bytes_consumed, 8);

        // Float number with ending non-digit characters
        let bytes = b"123.014 abc";
        let (token, bytes_consumed) = parse_number(bytes).unwrap();
        assert_eq!(token, NumberToken::Float64(123.014));
        assert_eq!(bytes_consumed, 7);

        // Float number with exponent 'e'
        let bytes = b"123e-2";
        let (token, bytes_consumed) = parse_number(bytes).unwrap();
        assert_eq!(token, NumberToken::Float64(1.23));
        assert_eq!(bytes_consumed, 6);

        // Float number with exponent 'E'
        let bytes = b"123E-2";
        let (token, bytes_consumed) = parse_number(bytes).unwrap();
        assert_eq!(token, NumberToken::Float64(1.23));
        assert_eq!(bytes_consumed, 6);

        // Float number ending with NameStart
        let bytes = b"123.014abc";
        let error = parse_number(bytes).unwrap_err();
        assert_eq!(error, Error::NameStart(b'a'));
    }

    #[test]
    fn test_integer() {
        // Integer number with a single digit integral part
        let bytes = b"1";
        let (token, bytes_consumed) = parse_number(bytes).unwrap();
        assert_eq!(token, NumberToken::Int64(1));
        assert_eq!(bytes_consumed, 1);

        // Integer number with multi digit integral part
        let bytes = b"123";
        let (token, bytes_consumed) = parse_number(bytes).unwrap();
        assert_eq!(token, NumberToken::Int64(123));
        assert_eq!(bytes_consumed, 3);

        // Integer number with negative integral part
        let bytes = b"-123";
        let (token, bytes_consumed) = parse_number(bytes).unwrap();
        assert_eq!(token, NumberToken::Int64(-123));
        assert_eq!(bytes_consumed, 4);

        // Integer number with ending non-digit characters
        let bytes = b"123 abc";
        let (token, bytes_consumed) = parse_number(bytes).unwrap();
        assert_eq!(token, NumberToken::Int64(123));
        assert_eq!(bytes_consumed, 3);

        // Integer number ending with NameStart
        let bytes = b"123abc";
        let error = parse_number(bytes).unwrap_err();
        assert_eq!(error, Error::NameStart(b'a'));
    }
}
