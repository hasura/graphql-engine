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
    let ctrl_c = async {
        tokio::signal::ctrl_c()
            .await
            .expect("failed to install Ctrl+C handler");
    };

    #[cfg(unix)]
    let terminate = async {
        tokio::signal::unix::signal(tokio::signal::unix::SignalKind::terminate())
            .expect("failed to install signal handler")
            .recv()
            .await;
    };

    #[cfg(not(unix))]
    let terminate = std::future::pending::<()>();

    tokio::select! {
        () = ctrl_c => {},
        () = terminate => {},
    }
}
