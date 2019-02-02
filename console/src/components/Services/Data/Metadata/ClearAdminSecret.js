import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { clearAdminSecretState } from '../../../AppState';
import globals from '../../../../Globals';

import {
  showSuccessNotification,
  showErrorNotification,
} from '../Notification';
import Button from '../../Layout/Button/Button';

class ClearAdminSecret extends Component {
  constructor() {
    super();
    this.state = {};
    this.state.isClearing = false;
  }
  render() {
    const metaDataStyles = require('./Metadata.scss');
    return (
      <div className={metaDataStyles.display_inline}>
        <Button
          data-test="data-clear-admin-secret"
          className={metaDataStyles.margin_right}
          color="white"
          size="sm"
          onClick={e => {
            e.preventDefault();
            this.setState({ isClearing: true });
            if (globals.isAdminSecretSet || globals.adminSecret) {
              clearAdminSecretState();
              this.props.dispatch(
                showSuccessNotification('Cleared Admin Secret')
              );
              this.setState({ isClearing: false });
              this.props.router.push('/login');
            } else {
              this.setState({ isClearing: false });
              const errorMessage = (
                <div style={{ padding: '5px' }}>
                  <div style={{ fontSize: '13px' }}>
                    No admin secret set or admin secret is set but isAdminSecretSet is
                    not set by the server
                  </div>
                  <br />
                  <div style={{ fontSize: '13px' }}>
                    Please look for <code>CONSOLE_ADMIN_SECRET</code> key under
                    window storage and delete it if it exists
                  </div>
                </div>
              );
              this.props.dispatch(showErrorNotification(errorMessage));
            }
          }}
        >
          {this.state.isClearing ? 'Clearing...' : 'Clear admin secret (logout)'}
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
