import React, { Component } from "react";

import OnlineUser from "./OnlineUser";

type userName = { name: string };
type State = {
  onlineUsers: Array<userName>
}

class OnlineUsersWrapper extends Component<State,State> {
  static defaultProps = {
    onlineUsers: [{ name: "someUser1"}, {name: "someUser2"}]
  } as State;

  constructor(props: State) {
    super(props);

    this.state = {
      onlineUsers: [
        { name: "someUser1" },
        { name: "someUser2" }
      ]
    };
  }

  render() {
    const onlineUsersList = [] as Array<React.FunctionComponentElement<any>>;
    this.state.onlineUsers.forEach((user, index) => {
      onlineUsersList.push(
        <OnlineUser
          user={user}
          key={index}
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
