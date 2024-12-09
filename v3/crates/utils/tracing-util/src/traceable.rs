use std::convert::Infallible;

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

// `Infallible` is used for errors that can never happen. It is therefore
// trivially traceable, as it will never occur.
impl TraceableError for Infallible {
    fn visibility(&self) -> ErrorVisibility {
        match *self {}
    }
}

/// An abstraction over values that can return an error, so that the error can
/// automatically be traced.
///
/// If the operation is guaranteed never to error, you can use [`Successful`].
pub trait Traceable {
    type ErrorType<'a>: TraceableError
    where
        Self: 'a;
    fn get_error(&self) -> Option<Self::ErrorType<'_>>;
}

/// A value which is always successful.
pub struct Successful<T>(T);

impl<T> Successful<T> {
    pub fn new(value: T) -> Self {
        Self(value)
    }

    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<T> From<T> for Successful<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}

impl<T> Traceable for Successful<T> {
    type ErrorType<'a>
        = Infallible
    where
        Self: 'a;

    fn get_error(&self) -> Option<Self::ErrorType<'_>> {
        None
    }
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

impl<E: TraceableError> TraceableError for ResultError<'_, E> {
    fn visibility(&self) -> ErrorVisibility {
        self.error.visibility()
    }
}

impl<R, E> Traceable for Result<R, E>
where
    E: TraceableError,
{
    type ErrorType<'a>
        = ResultError<'a, E>
    where
        R: 'a,
        E: 'a;

    fn get_error(&self) -> Option<ResultError<'_, E>> {
        self.as_ref().err().map(|e| ResultError { error: e })
    }
}
