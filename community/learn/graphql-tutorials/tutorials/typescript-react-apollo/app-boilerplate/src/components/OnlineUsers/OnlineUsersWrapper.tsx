import React from "react";
import OnlineUser from "./OnlineUser";

const OnlineUsersWrapper = () => {

  const online_users = [
    { name: "someUser1" },
    { name: "someUser2" }
  ];

  const onlineUsersList = online_users.map((user, index) => (
    <OnlineUser
      user={user}
      key={index}
    />)
  );

  return (
    <div className="onlineUsersWrapper">
      <div className="sliderHeader">
        Online users - {online_users.length}
      </div>
      { onlineUsersList }
    </div>
  );

}

export default OnlineUsersWrapper;
