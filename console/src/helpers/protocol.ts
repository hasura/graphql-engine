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
