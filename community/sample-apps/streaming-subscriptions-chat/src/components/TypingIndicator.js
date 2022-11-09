import React from 'react';
import { gql, useSubscription } from '@apollo/client';
import { StyledTypingIndicator } from '../styles/StyledChatApp';
import '../App.css';

const getUserTyping = gql`
  subscription ($selfId: Int) {
    user_typing(
      where: { id: { _neq: $selfId } }
      limit: 1
      order_by: { last_typed: desc }
    ) {
      last_typed
      username
    }
  }
`;

function TypingIndicator(props) {
  const { data, loading, error } = useSubscription(getUserTyping, {
    variables: {
      selfId: props.userId,
    },
  });
  if (loading) {
    return '';
  }
  if (error) {
    return '';
  }

  return (
    <StyledTypingIndicator>
      {data?.user_typing?.length === 0
        ? ``
        : `${data.user_typing[0].username} is typing ...`}
    </StyledTypingIndicator>
  );
}

export default TypingIndicator;
