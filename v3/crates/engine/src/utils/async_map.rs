use axum::async_trait;
use futures::Future;
use std::pin::Pin;

#[async_trait]
pub trait AsyncMap<T, U, F>
where
    F: FnOnce(T) -> Pin<Box<dyn Future<Output = U> + Send>> + Send,
{
    type Output;
    async fn async_map(self, map: F) -> Self::Output;
}

#[async_trait]
impl<T, U, F> AsyncMap<T, U, F> for std::option::Option<T>
where
    T: Send,
    U: Send,
    F: 'static + FnOnce(T) -> Pin<Box<dyn Future<Output = U> + Send>> + Send,
{
    type Output = std::option::Option<U>;
    async fn async_map(self, map: F) -> Self::Output {
        match self {
            Some(t) => {
                let u = map(t).await;
                Some(u)
            }
            None => None,
        }
    }
}
