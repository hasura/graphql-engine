import React, { Component } from "react";

import OnlineUser from "./OnlineUser";

class OnlineUsersWrapper extends Component {
  constructor() {
    super();

    this.state = {
      onlineUsers: [
        { name: "someUser1" },
        { name: "someUser2" }
      ]
    };
  }

  render() {
    const onlineUsersList = [];
    this.state.onlineUsers.forEach((user, index) => {
      onlineUsersList.push(
        <OnlineUser
          key={index}
          index={index}
          user={user}
        />);
    });

    return (
      <div className="onlineUsersWrapper">
        <div className="sliderHeader">
          Online users - {this.state.onlineUsers.length}
        </div>

        { onlineUsersList }
      </div>
    );
  }
}

export default OnlineUsersWrapper;
