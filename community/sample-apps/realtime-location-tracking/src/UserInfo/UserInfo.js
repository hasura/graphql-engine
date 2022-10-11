import React from 'react';
import PropTypes from 'prop-types';

import './UserInfo.css';

function UserInfo(props) {
  return (
    <div className="user_info">
      <div className="detail">
        <div className="onboarding">
          We've created a sample vehicle for this demo. Click on the following button to start tracking this vehicle's realtime location.
        </div>
        <div className="btn_wrapper">
          <button disabled={ props.isLoading ? true: false } onClick={ !props.isLoading ? props.handleTrackLocationClick : () => {}}>
            TRACK LOCATION
          </button>
        </div>
      </div>
    </div>
  )
}

UserInfo.propTypes = {
  userId: PropTypes.string.isRequired,
  isLoading: PropTypes.bool.isRequired,
  handleTrackLocationClick: PropTypes.func.isRequired,
};

export default UserInfo;
