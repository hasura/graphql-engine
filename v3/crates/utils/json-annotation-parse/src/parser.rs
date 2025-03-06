mod combinators;

mod types;
use combinators::{with_annotation, ws};
use nom::bytes::complete::escaped;
use nom::character::{
    complete::{char, multispace0, none_of},
    streaming::one_of,
};
use nom::combinator::{all_consuming, cut, map};
use nom::error::context;
use nom::sequence::{preceded, separated_pair, terminated};
pub use types::{Annotation, IResult, LocatedSpan};

use crate::types::Value;

use nom::branch::alt;
use nom::{bytes::complete::tag, multi::separated_list0, number::complete::double};

fn parse_false(input: LocatedSpan) -> IResult<Value<Annotation>> {
    map(ws(with_annotation(tag("false"))), |(ann, _)| {
        Value::Bool(ann, false)
    })(input)
}

fn parse_true(input: LocatedSpan) -> IResult<Value<Annotation>> {
    map(ws(with_annotation(tag("true"))), |(ann, _)| {
        Value::Bool(ann, true)
    })(input)
}

fn bool(input: LocatedSpan) -> IResult<Value<Annotation>> {
    (alt((parse_true, parse_false)))(input)
}

fn number(input: LocatedSpan) -> IResult<Value<Annotation>> {
    map(ws(with_annotation(double)), |(ann, float)| {
        Value::Number(ann, float)
    })(input)
}

// our main Value parser, basically, try all the parsers
fn value(input: LocatedSpan) -> IResult<Value<Annotation>> {
    alt((null, bool, string, number, array, object))(input)
}

fn raw_string(input: LocatedSpan) -> IResult<LocatedSpan> {
    let string_inner = escaped(none_of("\""), '\\', one_of("\"n\\"));

    context(
        "string",
        preceded(char('\"'), cut(terminated(string_inner, char('\"')))),
    )(input)
}

fn object(input: LocatedSpan) -> IResult<Value<Annotation>> {
    let key_value = separated_pair(ws(raw_string), cut(ws(char(':'))), ws(value));

    map(
        context(
            "map",
            with_annotation(preceded(
                char('{'),
                cut(terminated(
                    separated_list0(ws(char(',')), key_value),
                    ws(char('}')),
                )),
            )),
        ),
        |(ann, items)| {
            Value::Object(
                ann,
                items.into_iter().map(|(k, v)| (k.to_string(), v)).collect(),
            )
        },
    )(input)
}

fn array(input: LocatedSpan) -> IResult<Value<Annotation>> {
    let array_inner = ws(with_annotation(preceded(
        char('['),
        terminated(separated_list0(ws(char(',')), ws(value)), ws(char(']'))),
    )));

    map(array_inner, |(ann, items)| Value::Array(ann, items))(input)
}

fn string(input: LocatedSpan) -> IResult<Value<Annotation>> {
    let string_inner = escaped(none_of("\""), '\\', one_of("\"n\\"));

    map(
        with_annotation(context(
            "string",
            preceded(char('\"'), cut(terminated(string_inner, char('\"')))),
        )),
        |(ann, str)| Value::String(ann, str.to_string()),
    )(input)
}

fn null(input: LocatedSpan) -> IResult<Value<Annotation>> {
    map(ws(with_annotation(tag("null"))), |(ann, _)| {
        Value::Null(ann)
    })(input)
}

pub fn parse<'a, 'state>(source: &'a str) -> Result<Value<Annotation>, String>
where
    'state: 'a,
{
    let input = LocatedSpan::new(source);
    let (_, val) =
        all_consuming(terminated(value, multispace0))(input).map_err(|e| e.to_string())?;

    Ok(val)
}
