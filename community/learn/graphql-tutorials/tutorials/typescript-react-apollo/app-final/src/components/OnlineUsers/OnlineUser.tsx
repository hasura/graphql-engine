import * as React from 'react';
import { Online_Users } from '../../generated/graphql';

const OnlineUser = ({user}:Online_Users) => {
  return (
    <div className="userInfo">
      <div className="userImg">
        <i className="far fa-user" />
      </div>
      <div className="userName">
        {user ? user.name : null}
      </div>
    </div>
  );
};

export default OnlineUser;
