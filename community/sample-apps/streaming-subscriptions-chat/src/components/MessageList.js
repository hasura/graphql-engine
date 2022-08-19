import "../App.js";
import "../App.css";
import formatDistanceToNow from "date-fns/formatDistanceToNow";

export default function MessageList(props) {
  return (
    <div className={props.isNew ? "messageWrapperNew" : "messageWrapper"}>
      {props.messages.map((m) => {
        return (
          <div key={m.id} className="message">
            <div className="messageNameTime">
              <div className="messageName">
                <b>{m.username}</b>
              </div>
              <div className="messsageTime">
                <i>
                  {formatDistanceToNow(new Date(m.timestamp), {
                    addSuffix: true,
                  })}{" "}
                </i>
              </div>
            </div>
            <div className="messageText">{m.text}</div>
          </div>
        );
      })}
      <div style={{ height: 0 }} id="lastMessage"></div>
    </div>
  );
}
