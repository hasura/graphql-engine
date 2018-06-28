/* eslint-disable space-infix-ops */
/* eslint-disable no-loop-func  */

import PropTypes from 'prop-types';

import React, { Component } from 'react';
import { autoTrackRelations } from '../TableRelationships/Actions';

class AutoAddRelations extends Component {
  componentWillMount() {
    // Initialize
  }
  trackAllRelations = () => {
    this.props.dispatch(autoTrackRelations());
  };
  render() {
    // const styles = require('../PageContainer/PageContainer.scss');

    if (this.props.untrackedRelations.length === 0) {
      return null;
    }
    return (
      <div>
        <button
          onClick={this.trackAllRelations}
          className={'btn btn-xs btn-default'}
          data-test="track-all-relationships"
        >
          Track Available Relations
        </button>
      </div>
    );
  }
}

AutoAddRelations.propTypes = {
  untrackedRelations: PropTypes.array.isRequired,
  dispatch: PropTypes.func.isRequired,
};

export default AutoAddRelations;
