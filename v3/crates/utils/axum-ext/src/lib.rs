use std::future::Future;
// Connects a signal handler for the unix SIGTERM signal. (This is the standard signal that unix
// systems send when pressing ctrl+c or running `kill`. It is distinct from the "force kill" signal
// which is SIGKILL.) This function produces a future that resolves when a SIGTERM is received. We
// pass the future to axum's `with_graceful_shutdown` method to instruct axum to start a graceful
// shutdown when the signal is received.
//
// Listening for SIGTERM specifically avoids a 10-second delay when stopping the process.
//
// Also listens for tokio's cross-platform `ctrl_c` signal polyfill.
//
// copied from https://github.com/davidB/axum-tracing-opentelemetry/blob/main/examples/otlp/src/main.rs
pub async fn shutdown_signal() {
    // wait for a SIGINT, i.e. a Ctrl+C from the keyboard
    let sigint = async {
        tokio::signal::ctrl_c()
            .await
            .expect("failed to install signal handler");
    };

    // wait for a SIGTERM, i.e. a normal `kill` command
    #[cfg(unix)]
    let sigterm = async {
        tokio::signal::unix::signal(tokio::signal::unix::SignalKind::terminate())
            .expect("failed to install signal handler")
            .recv()
            .await
    };

    // on Unix, block until either of the above happens
    #[cfg(unix)]
    tokio::select! {
        () = sigint => (),
        _ = sigterm => (),
    }

    // only listen for Ctrl-C on Windows
    #[cfg(windows)]
    tokio::select! {
        _ = sigint => (),
    }
}

pub async fn shutdown_signal_with_handler<F, Fut>(shutdown_handler: F)
where
    F: FnOnce() -> Fut,
    Fut: Future<Output = ()>,
{
    // Wait for a shutdown signal
    shutdown_signal().await;

    // Invoke the shutdown handler
    shutdown_handler().await;
}
