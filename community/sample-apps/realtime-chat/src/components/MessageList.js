import '../App.js';
import '../App.css';
import moment from 'moment';

export default function MessageList(props) {
  return (
    <div className={props.isNew ? 'messageWrapperNew' : 'messageWrapper'}>
      {props.messages.map((m) => {
        return (
          <div key={m.id} className="message">
            <div className="messageNameTime">
              <div className="messageName">
                <b>{m.username}</b>
              </div>
              <div className="messsageTime">
                <i>{moment(m.timestamp).fromNow()} </i>
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
