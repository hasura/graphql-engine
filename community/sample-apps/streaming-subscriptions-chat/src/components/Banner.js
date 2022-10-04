import React from 'react';

import { StyledBanner } from '../styles/StyledChatApp';
import '../App.css';

const Banner = (props) => {
  return (
    <StyledBanner onClick={props.scrollToNewMessage}>
      You have {props.numOfNewMessages} new message(s)
    </StyledBanner>
  );
};

export default Banner;
