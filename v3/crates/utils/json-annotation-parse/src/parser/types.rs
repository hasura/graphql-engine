pub type IResult<'a, T> = nom::IResult<LocatedSpan<'a>, T>;

pub type LocatedSpan<'a> = nom_locate::LocatedSpan<&'a str>;

#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    pub offset: usize,
    pub row: u32,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
    pub start: Position,
    pub end: Position,
}
