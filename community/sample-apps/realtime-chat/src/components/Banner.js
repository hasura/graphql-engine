import React from 'react';
import '../App.css';

const Banner = (props) => {
  return (
    <div className="banner" onClick={props.scrollToNewMessage}>
      You have {props.numOfNewMessages} new message(s)
    </div>
  );
};

export default Banner;