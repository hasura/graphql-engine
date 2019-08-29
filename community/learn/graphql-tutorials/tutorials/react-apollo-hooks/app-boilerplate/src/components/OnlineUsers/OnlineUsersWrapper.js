import React, { useState } from "react";

import OnlineUser from "./OnlineUser";

const OnlineUsersWrapper = () => {
  const [onlineUsers, setOnlineUsers] = useState([
    { name: "someUser1" },
    { name: "someUser2" }
  ]);

  const onlineUsersList = [];
  onlineUsers.forEach((user, index) => {
    onlineUsersList.push(<OnlineUser key={index} index={index} user={user} />);
  });

  return (
    <div className="onlineUsersWrapper">
      <div className="sliderHeader">Online users - {onlineUsers.length}</div>
      {onlineUsersList}
    </div>
  );
};

export default OnlineUsersWrapper;
