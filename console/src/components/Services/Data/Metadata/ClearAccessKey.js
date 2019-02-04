import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { clearAccessKeyState } from '../../../AppState';
import globals from '../../../../Globals';

import {
  showSuccessNotification,
  showErrorNotification,
} from '../Notification';
import Button from '../../Layout/Button/Button';

class ClearAccessKey extends Component {
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
          data-test="data-clear-access-key"
          className={metaDataStyles.margin_right}
          color="white"
          size="sm"
          onClick={e => {
            e.preventDefault();
            this.setState({ isClearing: true });
            if (globals.isAccessKeySet || globals.accessKey) {
              clearAccessKeyState();
              this.props.dispatch(
                showSuccessNotification('Cleared Access Key')
              );
              this.setState({ isClearing: false });
              this.props.router.push('/login');
            } else {
              this.setState({ isClearing: false });
              const errorMessage = (
                <div style={{ padding: '5px' }}>
                  <div style={{ fontSize: '13px' }}>
                    No access key set or access key is set but isAccessKeySet is
                    not set by the server
                  </div>
                  <br />
                  <div style={{ fontSize: '13px' }}>
                    Please look for <code>CONSOLE_ACCESS_KEY</code> key under
                    window storage and delete it if it exists
                  </div>
                </div>
              );
              this.props.dispatch(showErrorNotification(errorMessage));
            }
          }}
        >
          {this.state.isClearing ? 'Clearing...' : 'Clear access key (logout)'}
        </Button>
      </div>
    );
  }
}

ClearAccessKey.propTypes = {
  dispatch: PropTypes.func.isRequired,
  dataHeaders: PropTypes.object.isRequired,
};

export default ClearAccessKey;
