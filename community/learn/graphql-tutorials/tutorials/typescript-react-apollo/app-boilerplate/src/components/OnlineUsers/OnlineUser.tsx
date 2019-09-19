import * as React from 'react';

type user = {
  name: string;
}

const OnlineUser = ({user}:{user: user}) => {
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
