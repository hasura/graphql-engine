/*
  Returns the right websocket protocol for the given URL protocol
  Pass window.location.protocol to this function
*/
export const getWebsocketProtocol = (windowLocationProtocol: string) => {
  if (windowLocationProtocol === 'https:') {
    return 'wss:';
  }
  return 'ws:';
};

/**
 * Replace the HTTP protocol in URL with WS.
 * @param url an HTTP URL
 * @returns a WebSocket URL.
 */
export const replaceURLHttpToWs = (url: string): string => {
  try {
    const urlVar = new URL(url);
    const websocketProtocol = getWebsocketProtocol(urlVar.protocol);
    urlVar.protocol = websocketProtocol;
    return urlVar.toString();
  } catch {
    const websocketProtocol = getWebsocketProtocol(window.location.protocol);
    return `${websocketProtocol}//${url.split('//')[1]}`;
  }
};
