import React, { useEffect, useRef, useState } from 'react';
import Markdown from 'markdown-to-jsx';
import DOMPurify from 'dompurify';
import './styles.css';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import { CloseIcon, RespondingIconGray, SparklesIcon } from '@site/src/components/AiChatBot/icons';
import { useLocalStorage } from 'usehooks-ts';
import profilePic from '@site/static/img/docs-bot-profile-pic.webp';
import { v4 as uuidv4 } from 'uuid';

interface Message {
  userMessage: string;
  botResponse: string;
}

interface Query {
  previousMessages: Message[];
  currentUserInput: string;
}

// Websocket Event data types (stringified)
// {  type: "loading", message: "Processing your request..." }
// {  type: "responsePart", message: "...part of response..." }
// {  type: "error", message: "error description" }
// {  type: "endOfStream", message: "End of stream..." }

const initialMessages: Message[] = [
  {
    userMessage: '',
    botResponse: "Hi! I'm DocsBot, the Hasura docs AI chatbot.",
  },
  {
    userMessage: '',
    botResponse: 'You can ask me anything about Hasura and I will try to answer.',
  },
  {
    userMessage: '',
    botResponse: 'Always check the docs for official information.',
  },
];

export function AiChatBot({ style }) {
  // Get the docsBotEndpointURL and hasuraVersion from the siteConfig
  const {
    siteConfig: { customFields },
  } = useDocusaurusContext();
  // Manage the open state of the popup
  const [isOpen, setIsOpen] = useState<boolean>(false);
  // Manage the bot responding state
  const [isResponding, setIsResponding] = useState<boolean>(false);
  // Manage the text input
  const [input, setInput] = useState<string>('');
  // Manage the message thread ID
  const [messageThreadId, setMessageThreadId] = useLocalStorage<String>(
    `hasuraV${customFields.hasuraVersion}ThreadId`,
    uuidv4()
  );
  // Manage the historical messages
  const [messages, setMessages] = useLocalStorage<Message[]>(
    `hasuraV${customFields.hasuraVersion}BotMessages`,
    initialMessages
  );
  // Manage the current message
  const [currentMessage, setCurrentMessage] = useState<Message>({ userMessage: '', botResponse: '' });
  // Manage scrolling to the end
  const [isAutoScroll, setIsAutoScroll] = useState<boolean>(true);
  // Manage the websocket
  const [ws, setWs] = useState<WebSocket | null>(null);
  // Set is Websocket connecting
  const [isConnecting, setIsConnecting] = useState<boolean>(true);

  // Use a ref because of the useEffect closure issue
  const currentMessageRef = useRef<Message>({ userMessage: '', botResponse: '' });

  // Enables scrolling to the end
  const scrollDiv = useRef<HTMLDivElement>(null);

  const { docsBotEndpointURL, hasuraVersion, DEV_TOKEN } = customFields as {
    docsBotEndpointURL: string;
    hasuraVersion: number;
    DEV_TOKEN: string;
  };

  const sanitizeInput = (input: string): string => {
    const sanitized = DOMPurify.sanitize(input.trim());
    return sanitized.replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#039;');
  };

  const validateInput = (input: string): boolean => {
    return input.length > 0 && input.length <= 1000;
  };

  const storedUserID = localStorage.getItem('hasuraDocsUserID') as string | 'null';

  // Effect to auto-scroll to the bottom if autoScroll is true
  useEffect(() => {
    if (isAutoScroll) {
      scrollDiv.current?.scrollTo({
        top: scrollDiv.current.scrollHeight,
        behavior: 'smooth',
      });
    }
  }, [currentMessage.botResponse]);

  // Detect if user scrolls up and disable auto-scrolling
  const handleScroll = e => {
    const atBottom =
      Math.abs(scrollDiv.current?.scrollHeight - Math.floor(e.target.scrollTop + e.target.clientHeight)) < 2;
    setIsAutoScroll(atBottom);
  };

  // Update the ref when the currentMessage changes ie: when the endpoint is responding
  useEffect(() => {
    currentMessageRef.current = currentMessage;
  }, [currentMessage]);

  // Manage the websocket and set event listener for messages
  useEffect(() => {
    let websocket;
    let reconnectInterval;

    const queryDevToken = process.env.NODE_ENV === 'development' && DEV_TOKEN ? `&devToken=${DEV_TOKEN}` : '';

    console.log('process.env.NODE_ENV', process.env.NODE_ENV);

    const connectWebSocket = () => {
      websocket = new WebSocket(
        encodeURI(`${docsBotEndpointURL}?version=${hasuraVersion}&userId=${storedUserID}${queryDevToken}`)
      );

      websocket.onopen = () => {
        console.log('Connected to the websocket');
        setIsConnecting(false);
        clearTimeout(reconnectInterval);
      };

      websocket.onmessage = event => {
        let response = { type: '', message: '' };

        try {
          response = JSON.parse(event.data) as { type: string; message: string };
        } catch (e) {
          console.error('error parsing websocket message', e);
        }

        switch (response.type) {
          case 'endOfStream': {
            console.log('end of stream');
            setMessages((prevMessages: Message[]) => [...prevMessages, currentMessageRef.current]);
            setCurrentMessage({ userMessage: '', botResponse: '' });
            setIsResponding(false);
            break;
          }
          case 'responsePart': {
            setIsResponding(true);
            setCurrentMessage(prevState => {
              return { ...prevState, botResponse: prevState?.botResponse + response.message };
            });
            break;
          }
          case 'error': {
            console.error('error', response.message);
            break;
          }
          case 'loading': {
            console.log('loading', response.message);
            break;
          }
          default: {
            console.error('unknown response type', response.type);
            break;
          }
        }
      };

      websocket.onclose = () => {
        console.log('WebSocket closed. Attempting to reconnect...');
        setIsConnecting(true);
        setIsResponding(false);
        reconnectInterval = setTimeout(connectWebSocket, 3000); // attempt to reconnect every 3 seconds
      };

      websocket.onerror = error => {
        console.error('WebSocket error:', error);
        setIsConnecting(true);
        setIsResponding(false);
        websocket.close();
      };

      setWs(websocket);
    };

    connectWebSocket();
    return () => {
      clearTimeout(reconnectInterval);
      if (websocket) {
        websocket.close();
      }
    };
  }, []);

  // Send the query to the websocket when the user submits the form
  const handleSubmit = async () => {
    const sanitizedInput = sanitizeInput(input);

    if (!validateInput(sanitizedInput)) {
      console.error('Invalid input');
      return;
    }

    if (ws) {
      const toSend = JSON.stringify({ previousMessages: messages, currentUserInput: input, messageThreadId });
      setCurrentMessage({ userMessage: sanitizedInput, botResponse: '' });
      setInput('');
      ws.send(toSend);
      setIsResponding(true);
    }
  };

  const renderMessage = (content: string) => {
    return (
      <Markdown
        options={{
          overrides: {
            a: {
              props: {
                target: '_blank',
                rel: 'noopener noreferrer'
              },
            },
          },
        }}
      >
        {DOMPurify.sanitize(content)}
      </Markdown>
    );
  };

  const isOnOverviewOrIndex =
    window.location.href.endsWith('/index') ||
    window.location.href.endsWith('/overview') ||
    window.location.href.endsWith('/overview/');

  return (
    <div className={'chat-popup'}>
      <div className={isOnOverviewOrIndex ? 'chat-popup-index-and-overviews' : 'chat-popup-other-pages'}>
        {isOpen ? (
          <></>
        ) : (
          <button className="open-chat-button" onClick={() => setIsOpen(!isOpen)}>
            {SparklesIcon} Hasura Docs AI Chat
          </button>
        )}
        {isOpen && (
          <div className={isOnOverviewOrIndex ? '' : 'absolute -bottom-11 w-full min-w-[500px] right-[10px]'}>
            {isOpen && (
              <button className="close-chat-button" onClick={() => setIsOpen(!isOpen)}>
                {CloseIcon} Close Chat
              </button>
            )}
            <div className="chat-window">
              <div className="info-bar">
                <div className={'bot-name-pic-container'}>
                  <div className="bot-name">DocsBot</div>
                  <img src={profilePic} height={30} width={30} className="bot-pic" />
                </div>
                <button
                  className="clear-button"
                  onClick={() => {
                    setMessages(initialMessages);
                    setCurrentMessage({ userMessage: '', botResponse: '' });
                    setMessageThreadId(uuidv4());
                  }}
                >
                  Clear
                </button>
              </div>
              <div className="messages-container" onScroll={handleScroll} ref={scrollDiv}>
                {messages.map((msg, index) => (
                  <div key={index}>
                    {msg.userMessage && (
                      <div className="user-message-container">
                        <div className="formatted-text message user-message">
                          {renderMessage(msg.userMessage)}
                        </div>
                      </div>
                    )}
                    {msg.botResponse && (
                      <div className="bot-message-container">
                        <div className="formatted-text message bot-message">
                          {renderMessage(msg.botResponse)}
                        </div>
                      </div>
                    )}
                  </div>
                ))}
                <div className="user-message-container">
                  {currentMessage.userMessage && (
                    <div className="formatted-text message user-message">
                      {renderMessage(currentMessage.userMessage)}
                    </div>
                  )}
                </div>
                <div>
                  <div className="bot-message-container">
                    {currentMessage.botResponse && (
                      <div className="formatted-text message bot-message">
                        {renderMessage(currentMessage.botResponse)}
                      </div>
                    )}
                  </div>
                  <div className="responding-div">{isResponding ? RespondingIconGray : null}</div>
                </div>
              </div>
              {/* Handles scrolling to the end */}
              {/*<div ref={messagesEndRef} />*/}
              <form
                className="input-container"
                onSubmit={e => {
                  e.preventDefault();
                  handleSubmit();
                }}
              >
                <input
                  disabled={isResponding || isConnecting}
                  className="input-text"
                  value={input}
                  onChange={e => setInput(e.target.value)}
                  maxLength={1000}
                />
                <button disabled={isResponding || isConnecting} className="input-button" type="submit">
                  {isConnecting ? 'Connecting...' : isResponding ? 'Responding...' : 'Send'}
                </button>
              </form>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}
