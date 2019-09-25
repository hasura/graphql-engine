import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { clearAdminSecretState } from '../../../AppState';

import { showSuccessNotification } from '../../Common/Notification';
import Button from '../../../Common/Button/Button';

class ClearAdminSecret extends Component {
  constructor() {
    super();

    this.state = {
      isClearing: false,
    };
  }

  render() {
    const metaDataStyles = require('../Settings.scss');

    const { dispatch } = this.props;
    const { isClearing } = this.state;

    return (
      <div className={metaDataStyles.display_inline}>
        <Button
          data-test="data-clear-access-key"
          className={metaDataStyles.margin_right}
          color="white"
          size="sm"
          onClick={e => {
            e.preventDefault();

            this.setState({ isClearing: true });

            clearAdminSecretState();

            dispatch(showSuccessNotification('Cleared admin-secret'));

            this.setState({ isClearing: false });

            this.props.router.push('/login');
          }}
        >
          {isClearing ? 'Clearing...' : 'Logout (clear admin-secret)'}
        </Button>
      </div>
    );
  }
}

ClearAdminSecret.propTypes = {
  dispatch: PropTypes.func.isRequired,
  dataHeaders: PropTypes.object.isRequired,
};

export default ClearAdminSecret;
