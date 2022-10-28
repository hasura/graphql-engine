import { useCallback, useEffect, useRef, useState } from 'react';
import { gql, useLazyQuery, useQuery, useSubscription } from '@apollo/client';

import '../App.js';
import Banner from './Banner';
import MessageList from './MessageList';
import { StyledMessagesList } from '../styles/StyledChatApp.js';

const fetchOldMessages = gql`
  query ($last_received_ts: timestamptz) {
    message(
      limit: 10
      order_by: { timestamp: desc }
      where: { timestamp: { _lt: $last_received_ts } }
    ) {
      id
      text
      username
      timestamp
    }
  }
`;

const fetchMessages = gql`
  {
    message(limit: 1, order_by: { timestamp: desc }, offset: 9) {
      id
      text
      username
      timestamp
    }
  }
`;

const subscribeToNewMessages = gql`
  subscription ($last_received_ts: timestamptz) {
    message_stream(
      cursor: { initial_value: { timestamp: $last_received_ts } }
      batch_size: 10
    ) {
      id
      username
      text
      timestamp
    }
  }
`;

export default function RenderMessages({
  setMutationCallback,
  username,
  userId,
  setDataStream,
}) {
  const [messages, setMessages] = useState([]);
  const [newMessages, setNewMessages] = useState([]);
  const [bottom, setBottom] = useState(true);
  const [initialLoad, setInitialLoad] = useState(false);
  const [initialTimestamp, setInitialTimestamp] = useState(
    '2018-08-21T19:58:46.987552+00:00'
  );

  const listInnerRef = useRef();

  // add old (read) messages to state
  const addOldMessages = (newMessages) => {
    const oldMessages = [...messages, ...newMessages];
    setMessages(oldMessages);
    setNewMessages([]);
  };

  const { loading } = useQuery(fetchMessages, {
    onCompleted: (data) => {
      addOldMessages(data.message);
      setInitialLoad(true);
      setInitialTimestamp(
        data.message[0]?.timestamp || '2018-08-21T19:58:46.987552+00:00'
      );
    },
  });

  const [loadOldMessages, { loading: loadingOldMessages }] = useLazyQuery(
    fetchOldMessages,
    {
      onCompleted: (data) => {
        setMessages([...[...data.message].reverse(), ...messages]);
      },
    }
  );

  useSubscription(subscribeToNewMessages, {
    skip: !initialLoad,
    variables: {
      last_received_ts: initialTimestamp,
    },
    onSubscriptionData: ({ subscriptionData }) => {
      if (!loading) {
        if (subscriptionData.data) {
          setDataStream(subscriptionData.data);
          if (!isViewScrollable()) {
            addOldMessages(subscriptionData.data.message_stream);
          } else {
            if (bottom) {
              addOldMessages(subscriptionData.data.message_stream);
            } else {
              addNewMessages(subscriptionData.data.message_stream);
            }
          }
        }
      }
    },
  });

  // scroll to bottom
  const scrollToBottom = () => {
    document
      ?.getElementById('lastMessage')
      ?.scrollIntoView({ behavior: 'instant' });
  };

  // scroll to the new message
  const scrollToNewMessage = () => {
    document
      ?.getElementById('newMessage')
      ?.scrollIntoView({ behavior: 'instant' });
  };

  if (newMessages.length === 0 && bottom) {
    scrollToBottom();
  }

  // add message to state when text is entered
  const mutationCallback = useCallback(() => {
    return (newMessage) => {
      const allMessages = [...messages, ...newMessages];
      allMessages.push(newMessage);
      setMessages(messages);
      setNewMessages([]);
    };
  }, [messages, newMessages]);

  // scroll handler
  const handleScroll = (e) => {
    const { scrollTop, scrollHeight, clientHeight } = listInnerRef.current;

    if (Math.abs(scrollTop + clientHeight - scrollHeight) < 10) {
      setBottom(true);
    } else {
      setBottom(false);
      if (bottom) {
      }
    }
  };

  useEffect(() => {
    setMutationCallback(mutationCallback);
  }, [setMutationCallback, mutationCallback]);

  // add new (unread) messages to state
  const addNewMessages = (incomingMessages) => {
    const allNewMessages = [...newMessages];
    incomingMessages.forEach((m) => {
      // do not add new messages from self
      if (m.username !== username) {
        allNewMessages.push(m);
      }
    });
    setNewMessages(allNewMessages);
  };

  // check if the view is scrollable
  const isViewScrollable = () => {
    const isInViewport = (elem) => {
      const bounding = elem.getBoundingClientRect();
      return (
        bounding.top >= 0 &&
        bounding.left >= 0 &&
        bounding.bottom <=
          (window.innerHeight || document.documentElement.clientHeight) &&
        bounding.right <=
          (window.innerWidth || document.documentElement.clientWidth)
      );
    };
    if (document.getElementById('lastMessage')) {
      return !isInViewport(document.getElementById('lastMessage'));
    }
    return false;
  };

  return (
    <StyledMessagesList onScroll={handleScroll} ref={listInnerRef}>
      {/* show "unread messages" banner if not at bottom */}
      {!bottom && newMessages.length > 0 && isViewScrollable() && (
        <Banner
          scrollToNewMessage={scrollToNewMessage}
          numOfNewMessages={newMessages.length}
        />
      )}

      {/* <div
        style={{
          margin: 'auto',
          textAlign: 'center',
        }}
      >
        <button
          onClick={() =>
            loadOldMessages({
              variables: { last_received_ts: messages[0].timestamp },
            })
          }
          disabled={loadingOldMessages}
        >
          {loadingOldMessages === true ? 'Loading...' : 'Load More'}{' '}
        </button>
      </div> */}

      {/* Render old messages */}
      <MessageList messages={messages} isNew={false} username={username} />
      {/* Show old/new message separation */}
      <div id="newMessage" className="oldNewSeparator">
        {newMessages.length !== 0 ? 'New messages' : null}
      </div>

      {/* render new messages */}
      <MessageList messages={newMessages} isNew={true} username={username} />
      {/* Bottom div to scroll to */}
    </StyledMessagesList>
  );
}
