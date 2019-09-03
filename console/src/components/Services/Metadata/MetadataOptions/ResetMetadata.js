import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import Button from '../../../Common/Button/Button';

import styles from '../../../Common/Common.scss';

import {
  showSuccessNotification,
  showErrorNotification,
} from '../../Common/Notification';
import { getConfirmation } from '../../../Common/utils/jsUtils';

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
        'This will permanently reset the GraphQL schema configuration and you will need to start from scratch';
      const isOk = getConfirmation(confirmMessage, true);
      if (!isOk) {
        return;
      }

      this.setState({ isResetting: true });

      const url = Endpoints.query;
      const requestBody = {
        type: 'clear_metadata',
        args: {},
      };
      const options = {
        method: 'POST',
        credentials: globalCookiePolicy,
        headers: {
          ...this.props.dataHeaders,
        },
        body: JSON.stringify(requestBody),
      };

      fetch(url, options).then(response => {
        response.json().then(data => {
          this.setState({ isResetting: false });

          if (response.ok) {
            dispatch(showSuccessNotification('Metadata reset successfully!'));
          } else {
            dispatch(
              showErrorNotification('Metadata reset failed', null, data)
            );
          }
        });
      });
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
