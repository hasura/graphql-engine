import { gql } from "@apollo/client";
import ChatWrapper from "./ChatWrapper";
import "../App.css";
import { useInterval } from "../hooks/useInterval";

const emitOnlineEvent = gql`
  mutation ($userId: Int!) {
    update_user_by_pk(
      pk_columns: { id: $userId }
      _set: { last_seen: "now()" }
    ) {
      id
    }
  }
`;

function Chat(props) {
  /**
   * Every 3 seconds emit an online event
   */
  useInterval(async () => {
    await props.client.mutate({
      mutation: emitOnlineEvent,
      variables: {
        userId: props.userId,
      },
    });
  }, 3000);

  return (
    <div>
      <ChatWrapper userId={props.userId} username={props.username} />
      <footer className="App-footer">
        <div className="hasura-logo">
          <img
            src="https://hasura.io/brand-assets/powered-by-hasura-primary-dark.svg"
            onClick={() => window.open("https://hasura.io")}
            alt="Powered by Hasura"
          />
          &nbsp; | &nbsp;
          <a
            href="https://github.com/hasura/graphql-engine/tree/master/community/sample-apps/streaming-subscriptions-chat"
            target="_blank"
            rel="noopener noreferrer"
          >
            Source
          </a>
          &nbsp; | &nbsp;
          <a
            href="https://hasura.io/blog/building-a-realtime-chat-app-with-graphql-subscriptions-d68cd33e73f"
            target="_blank"
            rel="noopener noreferrer"
          >
            Blogpost
          </a>
        </div>
        <div className="footer-small-text">
          <span>(The database resets every 24 hours)</span>
        </div>
      </footer>
    </div>
  );
}

export default Chat;
