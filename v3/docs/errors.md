# Error Management in Rust

This document offers recommendations for raising and handling exceptions. It
also outlines the design and usage of error types in Rust code.

## Table of Contents

- [Understanding the Result type](#understanding-the-result-type)
  - [The Question Mark operator](#the-question-mark-operator)
- [Designing Error Types](#designing-error-types)
  - [Scope-based Error handling](#scope-based-error-handling)
    - [Modules as Error Boundaries](#modules-as-error-boundaries)
  - [Categorizing Errors by Kind](#categorizing-errors-by-kind)

## Understanding the Result Type

Exceptions in Rust code are raised by returning a value of
[`Result<T, E>`](https://doc.rust-lang.org/std/result/enum.Result.html) type,
where `T` is the type of success value and `E` is the error type.

```rust
fn my_function() -> Result<T, E> {}
```

### The Question Mark operator

`Result` return values can be automatically unwrapped and chained using the `?`
operator, pronounced "try". This allows us to write code that looks flat, but
will actually return early with an error if one occurs. (Haskell programmers may
liken this to monadic binding with `>>=`; `?` is similar but with limited
ability)

You can use it as follows (imagine every function call return a
`Result<i32, ErrorType>`);

```rust
function display_very_important_data() -> Result<i32, ErrorType> {
    let user = authenticate_user()?;
    let data = retrieve_very_important_data_for(user)?;
    for item in data {
        emit(item)?;
    }
    Ok(data.len()) // you must wrap the return value if it is "pure"
}
```

For this to work, the errors must all be of same type, or they must be
convertible into the given error type way of `From` implementation. If neither
of these is the case, you can convert the error explicitly using
`.map_err(|err| ...)`.

## Designing Error Types

In a typical application code, errors can be of several kinds and are handled
differently. One approach is to model the error type as only a `String` value.
But it makes the classification of errors cumbersome while handling them.
Generally, the error types are enum types where each variant corresponds to a
specific kind of error.

```rust
enum Error {
    UserNotFound,
    InvalidPassword,
    PasswordNotMatched,
}
```

As the application grows, the error enum becomes overwhelmed with numerous
variants, making handling difficult.

```rust
fn handle_error(err: Error) {
    match err {
        Error::UserNotFound => {set_status(404)},
        Error::InvalidPassword => {set_status(400)},
        Error::PasswordNotMatched => {set_status(401)},
        // Code here will be expanded with more error variants.
    }
}
```

For reference, the GraphQL API execution layer in `engine` crate alone has a few
tens of errors in number.

It is essential to break down error enums with fewer variants for better
handling and readability. But, how do we break them? We need to understand the
errors broadly, such as where they originate (scope of error) and whether we can
group errors by any common attribute or kind. Let's discuss these aspects below.

### Scope-based Error handling

A scope within an application's code is defined as an isolated execution layer
that handles a specific task.

For instance, consider a user authentication application with several execution
layers responsible for request validation, fetching user information, password
management and response building. Each layer needs to produce its own error
type, encompassing only those variants raised within the layer. The
application's overarching error type should encompass variants representing
errors specific to each scope, alongside a handful of miscellaneous individual
errors.

```rust
#[derive(thiserror::Error, Debug)]
enum AppError {
    #[error("request error: {0}")]
    Request(#[from] RequestError),
    #[error("user error: {0}")]
    User(#[from] UserError),
    #[error("password error: {0}")]
    Password(#[from] PasswordError),
    #[error("response error: {0}")]
    Response(#[from] ResponseError),
    #[error("some other error occurred")]
    SomeOtherError,
}
```

The `#[from]` attribute will enable the conversion of errors into `AppError`
without the need to map and construct the variant. This conversion is handled
for you automatically by [using `?` operator](#the-question-mark-operator).

In rust, scopes within an application, generally, referred by modules and
submodules.

#### Modules as Error Boundaries

It is recommended to have a `error.rs` submodule to host all error-related code,
including types, traits etc. All functions in a module should only return the
error type specified by the module, thus restricting the usage of any external
errors.

```
|- user/
    |- error.rs
    |- mod.rs
    |- types.rs
```

```rust
// error.rs
pub enum UserError { ... }
```

```rust
// types.rs
pub struct UserInfo { ... }
```

```rust
// mod.rs
mod error;
mod types;
fn get_user_info(username: &str) -> Result<UserInfo, UserError> { ... }
```

### Categorizing Errors by Kind

If feasible, variants within an error type can be consolidated into a new error
type based on their similarities. For example, all unexpected internal errors
can be encompassed in an `enum InternalError { ... }` type.

```rust
#[derive(thiserror::Error, Debug)]
pub enum UserError {
    #[error("username {0} is not valid")]
    InvalidUserName(String),
    #[error("internal error: {0}")]
    Internal(#[from] InternalError),
}

#[derive(thiserror::Error, Debug)]
pub enum InternalError {
    #[error("DB exception: {0}")]
    DatabaseException(#[from] DBException),
}
```
