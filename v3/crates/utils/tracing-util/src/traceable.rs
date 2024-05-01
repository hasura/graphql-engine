pub enum ErrorVisibility {
    Internal,
    User,
}

pub trait TraceableError: core::fmt::Display + core::fmt::Debug {
    fn visibility(&self) -> ErrorVisibility;

    fn description(&self) -> String {
        self.to_string()
    }

    fn details(&self) -> String {
        format!("{self:?}")
    }
}

pub trait Traceable {
    type ErrorType<'a>: TraceableError
    where
        Self: 'a;
    fn get_error(&self) -> Option<Self::ErrorType<'_>>;
}

/// A helper type to wrap a reference to `E` from [`Result<T, E>`].
/// Only used as an associate type in [`Traceable`] implementation for [`Result<T, E>`].
#[derive(Debug)]
pub struct ResultError<'e, E> {
    error: &'e E,
}

impl<E: std::fmt::Display> std::fmt::Display for ResultError<'_, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.error.fmt(f)
    }
}

impl<'e, E: TraceableError> TraceableError for ResultError<'e, E> {
    fn visibility(&self) -> ErrorVisibility {
        self.error.visibility()
    }
}

impl<R, E> Traceable for Result<R, E>
where
    E: TraceableError,
{
    type ErrorType<'a> = ResultError<'a, E>
        where R: 'a, E: 'a;

    fn get_error(&self) -> Option<ResultError<'_, E>> {
        self.as_ref().err().map(|e| ResultError { error: e })
    }
}
