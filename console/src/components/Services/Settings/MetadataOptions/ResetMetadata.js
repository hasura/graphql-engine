import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Button from '../../../Common/Button/Button';

import styles from '../../../Common/Common.scss';

import { getConfirmation } from '../../../Common/utils/jsUtils';
import { resetMetadata } from '../../../../metadata/actions';

class ResetMetadata extends Component {
  constructor() {
    super();
    this.state = {};
    this.state.isResetting = false;
  }

  render() {
    const { dispatch } = this.props;

    const handleReset = e => {
      e.preventDefault();
      const confirmMessage =
        'This will permanently reset the Hasura metadata related to your tables, remote schemas, actions and triggers';
      const isOk = getConfirmation(confirmMessage, true);
      if (!isOk) {
        return;
      }
      const completionCallback = () => this.setState({ isResetting: false });
      this.setState({ isResetting: true });
      dispatch(resetMetadata(completionCallback, completionCallback));
    };

    return (
      <div className={styles.display_inline}>
        <Button
          data-test="data-reset-metadata"
          color="white"
          size="sm"
          onClick={handleReset}
        >
          {this.state.isResetting ? 'Resetting...' : 'Reset'}
        </Button>
      </div>
    );
  }
}

ResetMetadata.propTypes = {
  dispatch: PropTypes.func.isRequired,
  dataHeaders: PropTypes.object.isRequired,
};

export default ResetMetadata;
