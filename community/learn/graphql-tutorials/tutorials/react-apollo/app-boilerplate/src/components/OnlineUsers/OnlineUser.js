import React, { Component } from "react";
import PropTypes from "prop-types";

import "../../styles/App.css";

class OnlineUser extends Component {
  render() {
    const { user, index } = this.props;

    return (
      <div className="userInfo">
        <div className="userImg">
          <i className="far fa-user" />
        </div>
        <div data-test={index + "_" + user.name} className="userName">
          {user.name}
        </div>
      </div>
    );
  }
}

OnlineUser.propTypes = {
  user: PropTypes.object.isRequired,
  index: PropTypes.number.isRequired
};

export default OnlineUser;
