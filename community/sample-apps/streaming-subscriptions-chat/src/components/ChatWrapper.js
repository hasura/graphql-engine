import { useState } from 'react';
import RenderMessages from './RenderMessages';
import Textbox from './Textbox';
import OnlineUsers from './OnlineUsers';
import '../App.css';

export default function RenderMessagesProxy(props) {
  const [mutationCallback, setMutationCallback] = useState(null);

  return (
    <div className="chatWrapper">
      <div className="wd25 hidden-xs">
        <OnlineUsers userId={props.userId} username={props.username} />
      </div>
      <div className="mobileview visible-xs">
        <OnlineUsers userId={props.userId} username={props.username} />
      </div>
      <div className="wd75">
        <RenderMessages
          setMutationCallback={setMutationCallback}
          username={props.username}
          userId={props.userId}
        />
        <Textbox
          username={props.username}
          mutationCallback={mutationCallback}
          userId={props.userId}
        />
      </div>
    </div>
  );
}
