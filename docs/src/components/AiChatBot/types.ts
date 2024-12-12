export interface ClientMessage {
  userMessage: string;
  botResponse: string;
}

export interface BadBotResponse {
  messageId: string;
  responseText: string | null;
}

export interface ChatMessagePayload {
  payloadType: 'chatMessage';
  chatMessage: {
    previousMessages: ClientMessage[];
    currentUserInput: string;
    messageThreadId: string;
    messageId: string;
  };
  responseQuality: null;
}

export interface BadBotResponsePayload {
  payloadType: 'badBotResponse';
  badBotResponse: BadBotResponse;
  chatMessage: null;
}

export type WebsocketPayload = ChatMessagePayload | BadBotResponsePayload;

export interface BotWebsocketEvent {
  type: 'loading' | 'responsePart' | 'error' | 'endOfStream';
  message: string;
}
