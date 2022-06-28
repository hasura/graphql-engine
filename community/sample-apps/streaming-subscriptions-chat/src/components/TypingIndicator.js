import { gql, useSubscription } from '@apollo/client';
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
    <div className="typingIndicator">
      {data?.user_typing?.length === 0
        ? ''
        : `${data.user_typing[0].username} is typing ...`}
    </div>
  );
}

export default TypingIndicator;
