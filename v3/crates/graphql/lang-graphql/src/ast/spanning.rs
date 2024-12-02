use std::fmt;

use serde::{Deserialize, Serialize};
use thiserror::Error;

/// A reference to a line and column in an input source file
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub struct SourcePosition {
    line: usize,
    col: usize,
}

impl SourcePosition {
    #[doc(hidden)]
    pub fn new(line: usize, col: usize) -> SourcePosition {
        SourcePosition { line, col }
    }

    /// The line of the character in the input source
    ///
    /// One-based index: the first line is line one.
    pub fn line(&self) -> usize {
        self.line
    }

    /// The column of the character in the input source
    ///
    /// One-based index: the first column is column one.
    pub fn column(&self) -> usize {
        self.col
    }
}

impl fmt::Display for SourcePosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

/// Data structure used to wrap items with their start
#[derive(Error, Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct Positioned<T> {
    /// The wrapped item
    pub item: T,

    /// Start position of the item
    pub position: SourcePosition,
}

impl<T> Positioned<T> {
    #[doc(hidden)]
    pub fn new(position: &SourcePosition, item: T) -> Positioned<T> {
        Positioned {
            item,
            position: *position,
        }
    }

    /// Modify the contents of the spanned item
    pub fn map<O, F: Fn(T) -> O>(self, f: F) -> Positioned<O> {
        Positioned {
            item: f(self.item),
            position: self.position,
        }
    }
}

impl<T: fmt::Display> fmt::Display for Positioned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}. At: {}", self.item, self.position)
    }
}

// impl<T: std::error::Error> std::error::Error for Positioned<T> {}

/// Data structure used to wrap items with start and end markers in the input source
///
/// A "span" is a range of characters in the input source, starting at the
/// character pointed by the `start` field and ending just before the `end`
/// marker.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct Spanning<T> {
    /// The wrapped item
    pub item: T,

    /// Start position of the item
    pub start: SourcePosition,

    /// End position of the item
    pub end: SourcePosition,
}

impl<T> Spanning<T> {
    #[doc(hidden)]
    pub fn single_width(pos: &SourcePosition, item: T) -> Spanning<T> {
        let end = *pos;
        Spanning {
            item,
            start: *pos,
            end,
        }
    }

    #[doc(hidden)]
    pub fn start_end(start: SourcePosition, end: SourcePosition, item: T) -> Spanning<T> {
        Spanning { item, start, end }
    }

    #[doc(hidden)]
    #[allow(clippy::self_named_constructors)]
    pub fn spanning(v: Vec<Spanning<T>>) -> Option<Spanning<Vec<Spanning<T>>>> {
        if let (Some(start), Some(end)) = (v.first().map(|s| s.start), v.last().map(|s| s.end)) {
            Some(Spanning {
                item: v,
                start,
                end,
            })
        } else {
            None
        }
    }

    /// Modify the contents of the spanned item
    pub fn map<O, F: Fn(T) -> O>(self, f: F) -> Spanning<O> {
        Spanning {
            item: f(self.item),
            start: self.start,
            end: self.end,
        }
    }
}

impl<T: fmt::Display> fmt::Display for Spanning<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}. Start: {} End: {}", self.item, self.start, self.end)
    }
}
