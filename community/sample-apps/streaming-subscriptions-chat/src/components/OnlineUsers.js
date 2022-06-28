import { useState } from 'react';
import { gql, useSubscription } from '@apollo/client';

const fetchOnlineUsersSubscription = gql`
  subscription {
    user_online(order_by: { username: asc }) {
      id
      username
    }
  }
`;

function OnlineUsers() {
  const [showMobileView, setMobileView] = useState(false);

  const { data } = useSubscription(fetchOnlineUsersSubscription);

  const toggleMobileView = () => {
    setMobileView(!showMobileView);
  };

  const subscriptionData = (isMobileView) => (
    <div>
      <p
        className={isMobileView ? 'mobileuserListHeading' : 'userListHeading'}
        onClick={toggleMobileView}
      >
        Online Users ({!data?.user_online ? 0 : data?.user_online.length}){' '}
        {isMobileView && <i className="fa fa-angle-up"></i>}
      </p>
      {((isMobileView && showMobileView) || !isMobileView) && (
        <ul className={isMobileView ? 'mobileUserList' : 'userList'}>
          {data?.user_online.map((u) => {
            return <li key={u.id}>{u.username}</li>;
          })}
        </ul>
      )}
    </div>
  );

  return (
    <div>
      <div className="onlineUsers hidden-xs">{subscriptionData(false)}</div>
      <div className="mobileonlineUsers visible-xs">
        {subscriptionData(true)}
      </div>
    </div>
  );
}

export default OnlineUsers;
