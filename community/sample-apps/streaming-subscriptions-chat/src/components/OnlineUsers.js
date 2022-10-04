import React, { useState } from 'react';
import { gql, useSubscription } from '@apollo/client';

import {
  StyledLeftSection,
  StyledOnlineUsers,
  StyledOnlineUserCircle,
} from '../styles/StyledChatApp';
import { getUserBgColor } from './MessageList';

const fetchOnlineUsersSubscription = gql`
  subscription {
    user_online(order_by: { username: asc }) {
      id
      username
    }
  }
`;

function OnlineUsers({ dataStream }) {
  const [showMobileView, setMobileView] = useState(false);

  const [showMobileMenu, toggleMobileMenu] = useState(false);

  const { data } = useSubscription(fetchOnlineUsersSubscription);

  const toggleMobileView = () => {
    setMobileView(!showMobileView);
  };

  const subscriptionStreamData = (isMobileView) => (
    <StyledOnlineUsers>
      <p
        className={isMobileView ? 'mobileuserListHeading' : 'userListHeading'}
        onClick={toggleMobileView}
      >
        Subscription Stream
        {isMobileView && <i className="fa fa-angle-up"></i>}
      </p>
      {((isMobileView && showMobileView) || !isMobileView) && (
        <ul
          className={
            isMobileView ? 'mobileUserList' : 'userList subscription-stream'
          }
        >
          <p className="subscription-stream">
            {JSON.stringify(dataStream, undefined, 4)}
          </p>
        </ul>
      )}
    </StyledOnlineUsers>
  );

  const subscriptionData = (isMobileView) => (
    <StyledOnlineUsers>
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
            return (
              <StyledOnlineUserCircle
                bgColor={getUserBgColor(u.username)}
                key={u.id}
              >
                {u.username.substring(0, 2)}
              </StyledOnlineUserCircle>
            );
          })}
        </ul>
      )}
    </StyledOnlineUsers>
  );

  return (
    <StyledLeftSection showMobileMenu={showMobileMenu}>
      {/* Mobile View */}
      <div className="mobile-header hide-on-desk">
        <a href="https://hasura.io/">
          <img
            loading="lazy"
            alt="Hasura"
            src="https://graphql-engine-cdn.hasura.io/assets/main-site/logo_primary_light.svg"
            className="hasura-logo-img"
          />
        </a>
        {!showMobileMenu && (
          <div className="flex-div">
            <p onClick={() => toggleMobileMenu(true)}>
              Online Users&nbsp;(
              {!data?.user_online ? 0 : data?.user_online.length}){' '}
            </p>
            <i
              className={showMobileMenu ? 'fa fa-angle-down' : 'fa fa-angle-up'}
            ></i>
          </div>
        )}
        {showMobileMenu && (
          <div className="mobile-data-wrapper">
            <p className="close-btn" onClick={() => toggleMobileMenu(false)}>
              <i className="fa fa-times"></i>
            </p>
            <div className="onlineUsers">{subscriptionData(false)}</div>
            <div className="onlineUsers">{subscriptionStreamData(false)}</div>
          </div>
        )}
      </div>
      {/* ************ */}
      <div className="onlineUsers hideOnMobile">
        {subscriptionStreamData(false)}
      </div>
      <div className="onlineUsers hideOnMobile">{subscriptionData(false)}</div>
    </StyledLeftSection>
  );
}

export default OnlineUsers;
