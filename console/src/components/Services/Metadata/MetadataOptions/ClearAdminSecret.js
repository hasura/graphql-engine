import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { clearAdminSecretState, CONSOLE_ADMIN_SECRET } from '../../../AppState';
import globals from '../../../../Globals';

import {
  showSuccessNotification,
  showErrorNotification,
} from '../../Common/Notification';
import Button from '../../../Common/Button/Button';

class ClearAdminSecret extends Component {
  constructor() {
    super();
    this.state = {};
    this.state.isClearing = false;
  }
  render() {
    const metaDataStyles = require('../Metadata.scss');
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
            if (globals.isAdminSecretSet || globals.adminSecret) {
              clearAdminSecretState();
              this.props.dispatch(
                showSuccessNotification(`Cleared ${globals.adminSecretLabel}`)
              );
              this.setState({ isClearing: false });
              this.props.router.push('/login');
            } else {
              this.setState({ isClearing: false });
              const errorMessage = (
                <div style={{ padding: '5px' }}>
                  <div style={{ fontSize: '13px' }}>
                    No {globals.adminSecretLabel} set
                  </div>
                  <br />
                  <div style={{ fontSize: '13px' }}>
                    Please look for <code>{CONSOLE_ADMIN_SECRET}</code> key
                    under window storage and delete it if it exists
                  </div>
                </div>
              );
              this.props.dispatch(showErrorNotification(errorMessage));
            }
          }}
        >
          {this.state.isClearing
            ? 'Clearing...'
            : `Clear ${globals.adminSecretLabel} (logout)`}
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
