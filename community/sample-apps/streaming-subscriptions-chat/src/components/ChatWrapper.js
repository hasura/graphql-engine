import { useState } from 'react';
import styled from 'styled-components';

import {
  ThemeSwitch,
  StyledChatBox,
  StyledChatBoxFormDiv,
} from '../styles/StyledChatApp';
import { BetaAccessForm } from './BetaAccessForm';
import RenderMessages from './RenderMessages';
import Textbox from './Textbox';
import OnlineUsers from './OnlineUsers';
import '../App.css';

const StyledRightSection = styled.div`
  display: flex;
  flex-direction: column;
  width: 30%;
  height: 100%;
  padding: 24px 0;
  justify-content: space-between;

  @media (max-width: 1010px) {
    display: none;
  }
`;

const RightSection = (props) => {
  const handleOnChange = () => {
    props.toggleDarkTheme();
  };

  return (
    <StyledRightSection>
      <ThemeSwitch>
        <label id="switch" className="switch">
          <input
            type="checkbox"
            id="slider"
            onChange={handleOnChange}
            checked={props?.isDarkThemeActive ? false : true}
          />
          <span className="slider round"></span>
        </label>
      </ThemeSwitch>
      <BetaAccessForm {...props} />
    </StyledRightSection>
  );
};

export default function RenderMessagesProxy(props) {
  const [mutationCallback, setMutationCallback] = useState(null);
  const [dataStream, setDataStream] = useState(null);

  return (
    <div className="chatWrapper">
      <OnlineUsers
        userId={props.userId}
        username={props.username}
        dataStream={dataStream}
      />
      <StyledChatBox className="wd45">
        <StyledChatBoxFormDiv>
          <RenderMessages
            setMutationCallback={setMutationCallback}
            username={props.username}
            userId={props.userId}
            setDataStream={setDataStream}
          />
          <Textbox
            username={props.username}
            mutationCallback={mutationCallback}
            userId={props.userId}
          />
        </StyledChatBoxFormDiv>
      </StyledChatBox>
      <RightSection {...props} />
    </div>
  );
}
