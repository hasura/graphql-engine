import React, { Component } from "react";
import OnlineUser from "./OnlineUser";

class OnlineUsersWrapper extends Component {
  render() {
    const data = {
      online_users: [
        { name: "someUser1" },
        { name: "someUser2" }
      ]
    };

    const onlineUsersList = [];
    data.online_users.forEach((user, index) => {
      onlineUsersList.push(
        <OnlineUser
          key={index}
          index={index}
          user={user}
        />);
    });

    return (
      <div className="onlineUsersWrapper">
        <div className="sliderHeader">Online users - {data.online_users.length}</div>

        { onlineUsersList }
      </div>
    );
  }
}

export default OnlineUsersWrapper;
