//! Various utilities and extensions to futures/futures_utils
use futures_util::Future;

/// Execute an async function on all items in an iterator, in a concurrent
/// fashion
#[inline]
pub async fn execute_concurrently<T, U, F, Fut>(
    iterator: impl Iterator<Item = T>,
    // function to execute for each item
    f: F,
) -> Vec<U>
where
    F: Fn(T) -> Fut,
    Fut: Future<Output = U>,
{
    futures_util::future::join_all(iterator.map(f)).await
}

#[cfg(test)]
mod execute_utils_tests {
    use std::time::SystemTime;
    use tokio::time::{Duration, sleep};

    use super::*;

    #[tokio::test]
    async fn test_execute_concurrently() {
        let input = vec![1, 2, 2];
        let func = |item: u64| async move {
            sleep(Duration::from_secs(item)).await;
        };
        // if all of the tasks are executed concurrently, then the whole job
        // should take as long as the longest task, which here is 2 secs.
        let start = SystemTime::now();
        execute_concurrently(input.into_iter(), func).await;
        let end = SystemTime::now();
        let time_taken = end.duration_since(start).unwrap();
        assert_eq!(time_taken.as_secs(), 2);
    }
}
