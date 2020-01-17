/**
 * Returns true if the browser supports websockets otherwise return false.
 * @return {Boolean} is web-sockets supported.
 */
export const isWebsocketSupport = () => {
  if ('WebSocket' in window) {
    return true;
  }
  return false;
};

/**
 * Returns a WebSocket connect object.
 * @param  {String} url              The url which needs to be contacted.
 * @param  {Function} [onOpen=null]  The callback function which is called once the websocket connection is established.
 * @param  {Function} [onMessage=null] The callback function which receives one argument event. Use event.data for data of the event.
 * @param  {Function} [onClose=null]   The callback function which is called when the connection is closed.
 * @param  {Function} [onError=null]   The callback function which is called when an error occurs, receives one argument called error.
 * @return {WebSocket}                  The WebSocket object. Use send(data) to send data & close() to terminate the connection.
 */
export const establishWebSocketConn = (
  url,
  onOpen = null,
  onMessage = null,
  onClose = null,
  onError = null
) => {
  let conn = null;
  if (isWebsocketSupport()) {
    conn = new WebSocket(url);

    conn.onopen = onOpen;
    conn.onmessage = onMessage;
    conn.onclose = onClose;
    conn.onerror = onError;
  } else {
    // TODO implement a fallback which polls but mimics the interface of a WebSocket Object.
    console.error(
      'This console needs websockets and websockets is not supported by your browser.'
    );
    alert(
      'This console needs websockets and websockets is not supported by your browser.'
    );
  }
  return conn;
};

/**
 * Establish a websocket connection. This connection is more persistent and will re-establish the
 * connection if the connection is dropped by the server.
 *
 * @param  {String} url              The url which needs to be contacted.
 * @param  {Function} [onOpen=null]  The callback function which is called once the websocket connection is established.
 * @param  {Function} [onMessage=null] The callback function which receives one argument event. Use event.data for data of the event.
 * @param  {Function} [onClose=null]   The callback function which is called when the connection is closed.
 * @param  {Function} [onError=null]   The callback function which is called when an error occurs, receives one argument called error.
 * @return {WebSocket}                  The WebSocket object. Use send(data) to send data & close() to terminate the connection.
 */
export const permanentWebSocketConn = (
  url,
  onOpen = null,
  onMessage = null,
  onClose = null,
  onError = null
) => {
  // This is a closure instance (isolation of scope)
  return (() => {
    let retryOn = true;
    const noOfTurns = 10;
    let tries = 0;
    let conn = null;

    const onOpenHandler = () => {
      tries = 0;
      onOpen();
    };

    const onCloseHandler = () => {
      console.log('Connection closed by server.');
      const reConn = () => {
        if (retryOn) {
          try {
            conn = establishWebSocketConn(
              url,
              onOpenHandler,
              onMessage,
              onCloseHandler,
              onError
            );
          } catch (err) {
            console.error('Failed to establish websocket connection.');
          }
          tries++;
        }
      };
      // if retry is off, then call the user defined onClose handler.
      if (!retryOn) {
        onClose();
      } else {
        console.log('Re-connecting to websocket server.');
        const time = (tries / noOfTurns) * 2 + 2;
        setTimeout(
          () => {
            reConn();
          },
          time >= 60 ? 60000 : time * 1000
        );
      }
    };

    conn = establishWebSocketConn(
      url,
      onOpen,
      onMessage,
      onCloseHandler,
      onError
    );
    return {
      send: message => {
        conn.send(message);
      },
      close: () => {
        retryOn = false;
        conn.close();
      },
    };
  })();
};
