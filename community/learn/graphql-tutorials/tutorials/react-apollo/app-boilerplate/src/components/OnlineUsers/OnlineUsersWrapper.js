import React, { Component } from "react";

class OnlineUsersWrapper extends Component {
  render() {
    const data = {
      online_users: [
        { name: "someUser1" },
        { name: "someUser2" }
      ]
    };

    const usersList = [];
    data.online_users.forEach((user, index) => {
      usersList.push(
        <div key={user.name} className="userInfo">
          <div className="userImg">
            <i className="far fa-user" />
          </div>
          <div data-test={index + "_" + user.name} className="userName">
            {user.name}
          </div>
        </div>
      );
    });

    return (
      <div className="onlineUsersWrapper">
        <div className="sliderHeader">Online users - {data.online_users.length}</div>

        { usersList }
      </div>
    );
  }
}

export default OnlineUsersWrapper;
