use std::future::Future;
use std::pin::Pin;
use tokio::task;

/// Define a struct `Poller` that holds a `JoinHandle` for an asynchronous task
/// It is used to run a subscription operation in the background.
pub struct Poller {
    handle: task::JoinHandle<()>, // `JoinHandle` represents a handle to the async task, allowing us to await or abort it
}

impl Poller {
    /// Define a constructor for `Poller` that spawns an asynchronous task
    pub fn new<F>(async_fn: F) -> Self
    where
        // `F` is a generic type representing a function that, when called, returns a pinned boxed future
        F: FnOnce() -> Pin<Box<dyn Future<Output = ()> + Send>>,
    {
        // Spawn the asynchronous function provided and store the resulting handle
        let handle = task::spawn(async_fn());
        // Return a new `Poller` instance with the task handle
        Self { handle }
    }

    /// Method to stop the poller by aborting the running async task
    pub fn stop(self) {
        // Abort the task associated with the poller
        self.handle.abort();
    }
}
