/// Context for making HTTP requests
#[derive(Debug, Clone)]
pub struct HttpContext {
    /// The HTTP client to use for making requests
    pub client: reqwest::Client,
    /// Response size limit for NDC requests
    pub ndc_response_size_limit: Option<usize>,
}

#[derive(Clone, serde::Serialize, Debug)]
pub struct ProjectId(pub String);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ExposeInternalErrors {
    Expose,
    Censor,
}
