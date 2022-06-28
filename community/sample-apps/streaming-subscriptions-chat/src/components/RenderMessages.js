import { useCallback, useEffect, useState } from "react";
import { gql, useSubscription } from "@apollo/client";
import "../App.js";
import Banner from "./Banner";
import MessageList from "./MessageList";

const fetchMessages = gql`
  # We can pass in how far back we want to fetch messages
  subscription ($last_received_ts: timestamptz) {
    message_stream(
      cursor: { initial_value: { timestamp: $last_received_ts } }
      batch_size: 100
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
}) {
  const [messages, setMessages] = useState([]);
  const [newMessages, setNewMessages] = useState([]);
  const [bottom, setBottom] = useState(true);

  // add old (read) messages to state
  const addOldMessages = (newMessages) => {
    const oldMessages = [...messages, ...newMessages];
    setMessages(oldMessages);
    setNewMessages([]);
  };

  useSubscription(fetchMessages, {
    variables: {
      // Arbitrary large date to fetch all messages
      last_received_ts: "2018-08-21T19:58:46.987552+00:00",
    },
    onSubscriptionData: async ({ subscriptionData }) => {
      if (subscriptionData) {
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
    },
  });

  // scroll to bottom
  const scrollToBottom = () => {
    document
      ?.getElementById("lastMessage")
      ?.scrollIntoView({ behavior: "instant" });
  };

  // scroll to the new message
  const scrollToNewMessage = () => {
    document
      ?.getElementById("newMessage")
      ?.scrollIntoView({ behavior: "instant" });
  };

  if (newMessages.length === 0) {
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
  const handleScroll = useCallback(() => {
    return (e) => {
      const windowHeight =
        "innerHeight" in window
          ? window.innerHeight
          : document.documentElement.offsetHeight;
      const body = document.getElementById("chatbox");
      const html = document.documentElement;
      const docHeight = Math.max(
        body.scrollHeight,
        body.offsetHeight,
        html.clientHeight,
        html.scrollHeight,
        html.offsetHeight
      );
      const windowBottom = windowHeight + window.pageYOffset;
      if (windowBottom >= docHeight) {
        setBottom(true);
      } else {
        if (bottom) {
          setBottom(false);
        }
      }
    };
  }, [bottom]);

  useEffect(() => {
    window.addEventListener("scroll", handleScroll);
    return () => window.removeEventListener("scroll", handleScroll);
  }, [handleScroll]);

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
    setNewMessages(newMessages);
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
    if (document.getElementById("lastMessage")) {
      return !isInViewport(document.getElementById("lastMessage"));
    }
    return false;
  };

  return (
    <div id="chatbox">
      {/* show "unread messages" banner if not at bottom */}
      {!bottom && newMessages.length > 0 && isViewScrollable() ? (
        <Banner
          scrollToNewMessage={scrollToNewMessage}
          numOfNewMessages={newMessages.length}
        />
      ) : null}

      {/* Render old messages */}
      <MessageList messages={messages} isNew={false} username={username} />
      {/* Show old/new message separation */}
      <div id="newMessage" className="oldNewSeparator">
        {newMessages.length !== 0 ? "New messages" : null}
      </div>

      {/* render new messages */}
      <MessageList messages={newMessages} isNew={true} username={username} />
      {/* Bottom div to scroll to */}
    </div>
  );
}
