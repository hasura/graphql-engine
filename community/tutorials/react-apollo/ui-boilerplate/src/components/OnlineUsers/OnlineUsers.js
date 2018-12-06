import React, { Component } from "react";

class OnlineUsers extends Component {
  render() {
    const data = {online_users: [{ name: "someUser1" }, { name: "someUser2" }]};
    return (
      <div className="sliderMenu grayBgColor">
        <div className="sliderHeader">
          Online users - {data.online_users.length}
        </div>
        {data.online_users.map((user, index) => {
          return (
            <div key={user.name} className="userInfo">
              <div className="userImg">
                <i className="far fa-user" />
              </div>
              <div
                data-test={index + "_" + user.name}
                className="userName"
              >
                {user.name}
              </div>
            </div>
          );
        })}
      </div>
    );
  }
}

export default OnlineUsers;
