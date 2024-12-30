#[derive(Debug, PartialEq, Eq)]
pub enum LikeString {
    StartsWith(String),
    EndsWith(String),
    Contains(String),
}

/// Checks for three patterns:
///
/// 1. %{value}
/// 2. {value}%
/// 3. %{value}%
pub fn parse_like_string(like_str: &str, escape_char: char) -> Option<LikeString> {
    let escaped_like_str = escape_special_characters(like_str, escape_char);

    if let Some(suffix) = escaped_like_str.strip_prefix('%') {
        if !suffix.is_empty() {
            if let Some(substring) = suffix.strip_suffix('%') {
                if !substring.is_empty() {
                    return Some(LikeString::Contains(substring.to_string()));
                }
            } else {
                return Some(LikeString::EndsWith(suffix.to_string()));
            }
        }
    }

    if let Some(prefix) = escaped_like_str.strip_suffix('%') {
        if !prefix.is_empty() {
            return Some(LikeString::StartsWith(prefix.to_string()));
        }
    }

    None
}

fn escape_special_characters(like_str: &str, escape_char: char) -> String {
    let mut escaped_str = String::new();
    let mut chars = like_str.chars();

    while let Some(c) = chars.next() {
        if c == escape_char {
            if let Some(next_char) = chars.next() {
                // Escape the '%' and '_' characters
                if next_char == '%' || next_char == '_' || next_char == escape_char {
                    escaped_str.push(next_char);
                } else {
                    // If the next character isn't a special character, keep the escape character
                    escaped_str.push(escape_char);
                    escaped_str.push(next_char);
                }
            } else {
                escaped_str.push(escape_char);
            }
        } else {
            escaped_str.push(c);
        }
    }

    escaped_str
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_like_string_ends_with() {
        // Test input with "%value" (starts with)
        let result = parse_like_string("%hello", '\\');
        assert_eq!(result, Some(LikeString::EndsWith("hello".to_string())));

        // Test with "%value" but with an escape character
        let result = parse_like_string(r"%hello\%world", '\\');
        assert_eq!(
            result,
            Some(LikeString::EndsWith("hello%world".to_string()))
        );
    }

    #[test]
    fn test_parse_like_string_starts_with() {
        // Test input with "value%" (ends with)
        let result = parse_like_string("world%", '\\');
        assert_eq!(result, Some(LikeString::StartsWith("world".to_string())));

        // Test with "value%" and an escape character
        let result = parse_like_string(r"world\%hello%", '\\');
        assert_eq!(
            result,
            Some(LikeString::StartsWith("world%hello".to_string()))
        );
    }

    #[test]
    fn test_parse_like_string_contains() {
        // Test input with "%value%" (contains)
        let result = parse_like_string("%hello%", '\\');
        assert_eq!(result, Some(LikeString::Contains("hello".to_string())));

        // Test with "%value%" and an escape character
        let result = parse_like_string(r"%hello\%world%", '\\');
        assert_eq!(
            result,
            Some(LikeString::Contains("hello%world".to_string()))
        );
    }

    #[test]
    fn test_parse_like_string_no_pattern() {
        // Test input with no wildcard pattern
        let result = parse_like_string("hello", '\\');
        assert_eq!(result, None);
    }

    #[test]
    fn test_parse_like_string_empty_string() {
        // Test empty string
        let result = parse_like_string("", '\\');
        assert_eq!(result, None);
    }

    #[test]
    fn test_parse_like_string_empty_pattern() {
        // Test input with "%", which is effectively an empty pattern
        let result = parse_like_string("%", '\\');
        assert_eq!(result, None);
    }

    #[test]
    fn test_parse_like_string_escaped_percent_sign() {
        // Test that escaped % sign does not trigger a wildcard pattern
        let result = parse_like_string(r"%\%hello\%world\%%", '\\');
        assert_eq!(
            result,
            Some(LikeString::Contains(r"%hello%world%".to_string()))
        );
    }

    #[test]
    fn test_parse_like_string_multiple_escaped_characters() {
        // Test multiple escaped characters in the string
        let result = parse_like_string(r"%hello\%world%", '\\');
        assert_eq!(
            result,
            Some(LikeString::Contains("hello%world".to_string()))
        );
    }

    #[test]
    fn test_parse_like_string_escape_characters_in_the_middle() {
        // Test string with escape character in the middle
        let result = parse_like_string(r"abc\%def%", '\\');
        assert_eq!(result, Some(LikeString::StartsWith("abc%def".to_string())));
    }

    #[test]
    fn test_parse_like_string_escaped_underscore() {
        // Test escaped underscore (underscore is a wildcard in LIKE patterns)
        let result = parse_like_string(r"abc\_def%", '\\');
        assert_eq!(result, Some(LikeString::StartsWith("abc_def".to_string())));
    }

    #[test]
    fn test_parse_like_string_combined_escape_characters() {
        // Test combination of escaped characters
        let result = parse_like_string(r"abc\_def\%xyz%", '\\');
        assert_eq!(
            result,
            Some(LikeString::StartsWith("abc_def%xyz".to_string()))
        );
    }

    #[test]
    fn test_parse_like_string_ends_with_with_dollar_escape() {
        // Test input with "%value" (starts with) using $ as the escape character
        let result = parse_like_string("%hello", '$');
        assert_eq!(result, Some(LikeString::EndsWith("hello".to_string())));

        // Test with "$value" and an escaped special character
        let result = parse_like_string(r"%hello$%world", '$');
        assert_eq!(
            result,
            Some(LikeString::EndsWith("hello%world".to_string()))
        );
    }

    #[test]
    fn test_parse_like_string_starts_with_with_dollar_escape() {
        // Test with "value$%" and an escaped special character
        let result = parse_like_string(r"world$%hello%", '$');
        assert_eq!(
            result,
            Some(LikeString::StartsWith("world%hello".to_string()))
        );
    }

    #[test]
    fn test_parse_like_string_no_pattern_with_dollar_escape() {
        // Test input with no wildcard pattern and escape character $ (should return None)
        let result = parse_like_string("hello", '$');
        assert_eq!(result, None);
    }

    #[test]
    fn test_parse_like_string_empty_string_with_dollar_escape() {
        // Test empty string with $ escape character
        let result = parse_like_string("", '$');
        assert_eq!(result, None);
    }

    #[test]
    fn test_parse_like_string_empty_pattern_with_dollar_escape() {
        // Test input with "$", which is effectively an empty pattern
        let result = parse_like_string("%", '$');
        assert_eq!(result, None);
    }

    #[test]
    fn test_parse_like_string_escaped_percent_sign_with_dollar_escape() {
        // Test that escaped % sign using $ does not trigger a wildcard pattern
        let result = parse_like_string(r"%$%hello$%world$%%", '$');
        assert_eq!(
            result,
            Some(LikeString::Contains(r"%hello%world%".to_string()))
        );
    }

    #[test]
    fn test_parse_like_string_multiple_escaped_characters_with_dollar_escape() {
        // Test multiple escaped characters using $ escape character
        let result = parse_like_string(r"%hello$%world$%%", '$');
        assert_eq!(
            result,
            Some(LikeString::Contains("hello%world%".to_string()))
        );
    }

    #[test]
    fn test_parse_like_string_escape_characters_in_the_middle_with_dollar_escape() {
        // Test string with escape character in the middle using $ escape character
        let result = parse_like_string(r"abc$%def$%", '$');
        assert_eq!(result, Some(LikeString::StartsWith("abc%def".to_string())));
    }

    #[test]
    fn test_parse_like_string_escaped_underscore_with_dollar_escape() {
        // Test escaped underscore (underscore is a wildcard in LIKE patterns) with $ escape character
        let result = parse_like_string(r"abc$_def$%", '$');
        assert_eq!(result, Some(LikeString::StartsWith("abc_def".to_string())));
    }

    #[test]
    fn test_parse_like_string_combined_escape_characters_with_dollar_escape() {
        // Test combination of escaped characters using $ escape character
        let result = parse_like_string(r"abc$_def$%xyz$%", '$');
        assert_eq!(
            result,
            Some(LikeString::StartsWith("abc_def%xyz".to_string()))
        );
    }
}
