import * as React from 'react';

type User = {
  user: { name: string }
}

const OnlineUser = ({user}: User) => {
  return (
    <div className="userInfo">
      <div className="userImg">
        <i className="far fa-user" />
      </div>
      <div className="userName">
        {user.name}
      </div>
    </div>
  );
};

export default OnlineUser;
