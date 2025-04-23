#[derive(Debug, Clone)]
pub enum EndPoint {
    V1Rest,
    V1Jsonapi,
}

impl EndPoint {
    pub fn as_str(&self) -> &'static str {
        match self {
            EndPoint::V1Rest => "/v1/rest",
            EndPoint::V1Jsonapi => "/v1/jsonapi",
        }
    }
}
