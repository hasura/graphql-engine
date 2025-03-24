use ndc_models::{CollectionName, FieldName};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::hash::{Hash, Hasher};
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash, Serialize, Deserialize, JsonSchema)]
#[serde(tag = "type")]
pub enum Rel {
    From {
        collection: CollectionName,
        columns: Vec<FieldName>,
    },
    Limit {
        input: Arc<Rel>,
        fetch: Option<usize>,
        skip: usize,
    },
    Project {
        input: Arc<Rel>,
        exprs: Vec<Expression>,
    },
    Filter {
        input: Arc<Rel>,
        predicate: Expression,
    },
    Sort {
        input: Arc<Rel>,
        exprs: Vec<Sort>,
    },
    Join {
        left: Arc<Rel>,
        right: Arc<Rel>,
        on: Vec<JoinOn>,
        join_type: JoinType,
    },
    Aggregate {
        input: Arc<Rel>,
        group_by: Vec<Expression>,
        aggregates: Vec<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash, Serialize, Deserialize, JsonSchema)]
pub struct JoinOn {
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash, Serialize, Deserialize, JsonSchema)]
pub enum JoinType {
    Left,
    Right,
    Inner,
    Full,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash, Serialize, Deserialize, JsonSchema)]
pub struct Sort {
    pub expr: Expression,
    pub asc: bool,
    pub nulls_first: bool,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash, Serialize, Deserialize, JsonSchema)]
pub struct CaseWhen {
    pub when: Box<Expression>,
    pub then: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize, JsonSchema)]
#[serde(tag = "type")]
pub enum Literal {
    Null,
    Boolean {
        value: Option<bool>,
    },
    /// 32bit float
    Float32 {
        value: Option<f32>,
    },
    /// 64bit float
    Float64 {
        value: Option<f64>,
    },
    /// signed 8bit int
    Int8 {
        value: Option<i8>,
    },
    /// signed 16bit int
    Int16 {
        value: Option<i16>,
    },
    /// signed 32bit int
    Int32 {
        value: Option<i32>,
    },
    /// signed 64bit int
    Int64 {
        value: Option<i64>,
    },
    /// unsigned 8bit int
    UInt8 {
        value: Option<u8>,
    },
    /// unsigned 16bit int
    UInt16 {
        value: Option<u16>,
    },
    /// unsigned 32bit int
    UInt32 {
        value: Option<u32>,
    },
    /// unsigned 64bit int
    UInt64 {
        value: Option<u64>,
    },
    /// 128-bit decimal
    Decimal128 {
        value: Option<i128>,
        scale: u8,
        prec: i8,
    },
    /// 256-bit decimal
    Decimal256 {
        value: Option<String>,
        scale: u8,
        prec: i8,
    },
    /// utf-8 encoded string.
    Utf8 {
        value: Option<String>,
    },
    /// Date stored as a signed 32bit int days since UNIX epoch 1970-01-01
    Date32 {
        value: Option<i32>,
    },
    /// Date stored as a signed 64bit int milliseconds since UNIX epoch 1970-01-01
    Date64 {
        value: Option<i64>,
    },
    /// Time stored as a signed 32bit int as seconds since midnight
    Time32Second {
        value: Option<i32>,
    },
    /// Time stored as a signed 32bit int as milliseconds since midnight
    Time32Millisecond {
        value: Option<i32>,
    },
    /// Time stored as a signed 64bit int as microseconds since midnight
    Time64Microsecond {
        value: Option<i64>,
    },
    /// Time stored as a signed 64bit int as nanoseconds since midnight
    Time64Nanosecond {
        value: Option<i64>,
    },
    /// Timestamp Second
    TimestampSecond {
        value: Option<i64>,
    },
    /// Timestamp Milliseconds
    TimestampMillisecond {
        value: Option<i64>,
    },
    /// Timestamp Microseconds
    TimestampMicrosecond {
        value: Option<i64>,
    },
    /// Timestamp Nanoseconds
    TimestampNanosecond {
        value: Option<i64>,
    },
    /// Duration in seconds
    DurationSecond {
        value: Option<i64>,
    },
    /// Duration in milliseconds
    DurationMillisecond {
        value: Option<i64>,
    },
    /// Duration in microseconds
    DurationMicrosecond {
        value: Option<i64>,
    },
    /// Duration in nanoseconds
    DurationNanosecond {
        value: Option<i64>,
    },
}

// Hack
impl Eq for Literal {}

impl Hash for Literal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Literal::Null => 0.hash(state),
            Literal::Boolean { value } => {
                1.hash(state);
                value.hash(state);
            }
            Literal::Float32 { value } => {
                2.hash(state);
                value.map(|f| f32::to_string(&f)).hash(state);
            }
            Literal::Float64 { value } => {
                3.hash(state);
                value.map(|f| f64::to_string(&f)).hash(state);
            }
            Literal::Int8 { value } => {
                4.hash(state);
                value.hash(state);
            }
            Literal::Int16 { value } => {
                5.hash(state);
                value.hash(state);
            }
            Literal::Int32 { value } => {
                6.hash(state);
                value.hash(state);
            }
            Literal::Int64 { value } => {
                7.hash(state);
                value.hash(state);
            }
            Literal::UInt8 { value } => {
                8.hash(state);
                value.hash(state);
            }
            Literal::UInt16 { value } => {
                9.hash(state);
                value.hash(state);
            }
            Literal::UInt32 { value } => {
                10.hash(state);
                value.hash(state);
            }
            Literal::UInt64 { value } => {
                11.hash(state);
                value.hash(state);
            }
            Literal::Decimal128 { value, scale, prec } => {
                12.hash(state);
                value.hash(state);
                scale.hash(state);
                prec.hash(state);
            }
            Literal::Decimal256 { value, scale, prec } => {
                13.hash(state);
                value.hash(state);
                scale.hash(state);
                prec.hash(state);
            }
            Literal::Utf8 { value } => {
                14.hash(state);
                value.hash(state);
            }
            Literal::Date32 { value } => {
                15.hash(state);
                value.hash(state);
            }
            Literal::Date64 { value } => {
                16.hash(state);
                value.hash(state);
            }
            Literal::Time32Second { value } => {
                17.hash(state);
                value.hash(state);
            }
            Literal::Time32Millisecond { value } => {
                18.hash(state);
                value.hash(state);
            }
            Literal::Time64Microsecond { value } => {
                19.hash(state);
                value.hash(state);
            }
            Literal::Time64Nanosecond { value } => {
                20.hash(state);
                value.hash(state);
            }
            Literal::TimestampSecond { value } => {
                21.hash(state);
                value.hash(state);
            }
            Literal::TimestampMillisecond { value } => {
                22.hash(state);
                value.hash(state);
            }
            Literal::TimestampMicrosecond { value } => {
                23.hash(state);
                value.hash(state);
            }
            Literal::TimestampNanosecond { value } => {
                24.hash(state);
                value.hash(state);
            }
            Literal::DurationSecond { value } => {
                25.hash(state);
                value.hash(state);
            }
            Literal::DurationMillisecond { value } => {
                26.hash(state);
                value.hash(state);
            }
            Literal::DurationMicrosecond { value } => {
                27.hash(state);
                value.hash(state);
            }
            Literal::DurationNanosecond { value } => {
                28.hash(state);
                value.hash(state);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash, Serialize, Deserialize, JsonSchema)]
#[serde(tag = "type")]
pub enum ScalarType {
    Null,
    Boolean,
    Float32,
    Float64,
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Decimal128 { scale: u8, prec: i8 },
    Decimal256 { scale: u8, prec: i8 },
    Utf8,
    Date32,
    Date64,
    Time32Second,
    Time32Millisecond,
    Time64Microsecond,
    Time64Nanosecond,
    TimestampSecond,
    TimestampMillisecond,
    TimestampMicrosecond,
    TimestampNanosecond,
    DurationSecond,
    DurationMillisecond,
    DurationMicrosecond,
    DurationNanosecond,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash, Serialize, Deserialize, JsonSchema)]
#[serde(tag = "type")]
pub enum Expression {
    Literal {
        literal: Literal,
    },
    Column {
        index: usize,
    },
    Cast {
        expr: Box<Expression>,
        as_type: ScalarType,
    },
    TryCast {
        expr: Box<Expression>,
        as_type: ScalarType,
    },
    Case {
        when: Vec<CaseWhen>,
        default: Option<Box<Expression>>,
    },

    // Binary operators
    And {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Or {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Eq {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    NotEq {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Lt {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    LtEq {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Gt {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    GtEq {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Plus {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Minus {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Multiply {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Divide {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Modulo {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Like {
        expr: Box<Expression>,
        pattern: Box<Expression>,
    },
    ILike {
        expr: Box<Expression>,
        pattern: Box<Expression>,
    },
    NotLike {
        expr: Box<Expression>,
        pattern: Box<Expression>,
    },
    NotILike {
        expr: Box<Expression>,
        pattern: Box<Expression>,
    },

    // Unary operators
    Not {
        expr: Box<Expression>,
    },
    IsNotNull {
        expr: Box<Expression>,
    },
    IsNull {
        expr: Box<Expression>,
    },
    IsTrue {
        expr: Box<Expression>,
    },
    IsFalse {
        expr: Box<Expression>,
    },
    IsUnknown {
        expr: Box<Expression>,
    },
    IsNotTrue {
        expr: Box<Expression>,
    },
    IsNotFalse {
        expr: Box<Expression>,
    },
    IsNotUnknown {
        expr: Box<Expression>,
    },
    Negative {
        expr: Box<Expression>,
    },

    // Other operators
    Between {
        low: Box<Expression>,
        expr: Box<Expression>,
        high: Box<Expression>,
    },
    NotBetween {
        low: Box<Expression>,
        expr: Box<Expression>,
        high: Box<Expression>,
    },
    In {
        expr: Box<Expression>,
        list: Vec<Expression>,
    },
    NotIn {
        expr: Box<Expression>,
        list: Vec<Expression>,
    },

    // Scalar functions
    ToLower {
        expr: Box<Expression>,
    },
    ToUpper {
        expr: Box<Expression>,
    },

    // abs
    // acos
    // acosh
    // ascii
    // asin
    // asinh
    // atan
    // atan2
    // atanh
    // bit_length
    // btrim
    // cbrt
    // ceil
    // char_length
    // character_length
    // chr
    // coalesce
    // concat
    // concat_ws
    // contains
    // cos
    // cosh
    // cot
    // current_date
    // current_time
    // current_timestamp
    // date_bin
    // date_format
    // date_part
    // date_trunc
    // datepart
    // datetrunc
    // decode
    // degrees
    // digest
    // encode
    // ends_with
    // exp
    // factorial
    // find_in_set
    // floor
    // from_unixtime
    // gcd
    // greatest
    // ifnull
    // initcap
    // instr
    // isnan
    // iszero
    // lcm
    // least
    // left
    // length
    // levenshtein
    // ln
    // log
    // log10
    // log2
    // lower
    // lpad
    // ltrim
    // make_date
    // md5
    // nanvl
    // now
    // nullif
    // nvl
    // nvl2
    // octet_length
    // pi
    // position
    // pow
    // power
    // radians
    // random
    // regexp_count
    // regexp_like
    // regexp_match
    // regexp_replace
    // repeat
    // replace
    // reverse
    // right
    // round
    // rpad
    // rtrim
    // sha224
    // sha256
    // sha384
    // sha512
    // signum
    // sin
    // sinh
    // split_part
    // sqrt
    // starts_with
    // strpos
    // substr
    // substr_index
    // substring
    // substring_index
    // tan
    // tanh
    // to_char
    // to_date
    // to_hex
    // to_local_time
    // to_timestamp
    // to_timestamp_micros
    // to_timestamp_millis
    // to_timestamp_nanos
    // to_timestamp_seconds
    // to_unixtime
    // today
    // translate
    // trim
    // trunc
    // upper
    // uuid

    // Aggregate functions
    Average {
        expr: Box<Expression>,
    },
    BoolAnd {
        expr: Box<Expression>,
    },
    BoolOr {
        expr: Box<Expression>,
    },
    Count {
        expr: Box<Expression>,
    },
    FirstValue {
        expr: Box<Expression>,
    },
    LastValue {
        expr: Box<Expression>,
    },
    Max {
        expr: Box<Expression>,
    },
    Mean {
        expr: Box<Expression>,
    },
    Median {
        expr: Box<Expression>,
    },
    Min {
        expr: Box<Expression>,
    },
    StringAgg {
        expr: Box<Expression>,
    },
    Sum {
        expr: Box<Expression>,
    },
    Var {
        expr: Box<Expression>,
    },
    // array_agg
    // bit_and
    // bit_or
    // bit_xor
    // grouping
    // var_pop
    // var_population
    // var_samp
    // var_sample

    // Window functions

    // cume_dist
    // dense_rank
    // first_value
    // lag
    // last_value
    // lead
    // nth_value
    // ntile
    // percent_rank
    // rank
    // row_number
}
