import '../App.js';
import '../App.css';
import formatDistanceToNow from 'date-fns/formatDistanceToNow';

import { StyledMessage } from '../styles/StyledChatApp.js';

const alphabetsArr = [
  'a',
  'b',
  'c',
  'd',
  'e',
  'f',
  'g',
  'h',
  'i',
  'j',
  'k',
  'l',
  'm',
  'n',
  'o',
  'p',
  'q',
  'r',
  's',
  't',
  'u',
  'v',
  'w',
  'x',
  'y',
  'z',
];

export const getUserBgColor = (username) => {
  if (username && username?.constructor?.name === 'String') {
    const firstChar = username.charAt(0).toLowerCase();

    const charIndex = alphabetsArr.indexOf(firstChar);

    if (charIndex <= 4) {
      return '#638FFF';
    }

    if (charIndex <= 8) {
      return '#F669A1';
    }

    if (charIndex <= 12) {
      return '#A36FF8';
    }

    if (charIndex <= 16) {
      return '#FFC960';
    }

    if (charIndex <= 20) {
      return '#39DAAA';
    }

    if (charIndex <= 27) {
      return '#45D7F6';
    }
  }

  return '#4F6C86';
};

export default function MessageList(props) {
  return (
    <div className={props.isNew ? 'messageWrapperNew' : 'messageWrapper'}>
      {props.messages.map((m) => {
        return (
          <StyledMessage key={m.id} bgColor={getUserBgColor(m.username)}>
            <div className="messageNameTime">
              <div className="messageName">{m.username.substring(0, 2)}</div>
              <div className="messageText">{m.text}</div>
            </div>
            <div className="time_stamp">
              {formatDistanceToNow(new Date(m.timestamp), {
                addSuffix: true,
              })}{' '}
            </div>
          </StyledMessage>
        );
      })}
      <div style={{ height: 0 }} id="lastMessage"></div>
    </div>
  );
}
