import React, { Component } from 'react';
import PropTypes from 'prop-types';

class ApiRequestDetails extends Component {
  render() {
    return (
      <div className="pt-md">
        <div>{this.props.title}</div>
        <div>{this.props.description}</div>
      </div>
    );
  }
}

ApiRequestDetails.propTypes = {
  title: PropTypes.string.isRequired,
  description: PropTypes.string.isRequired,
};

export default ApiRequestDetails;
