import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { clearAccessKeyState } from '../../../AppState';
import globals from '../../../../Globals';

import {
  showSuccessNotification,
  showErrorNotification,
} from '../Notification';

class ClearAccessKey extends Component {
  constructor() {
    super();
    this.state = {};
    this.state.isClearing = false;
  }
  render() {
    const styles = require('../PageContainer/PageContainer.scss');
    const metaDataStyles = require('./Metadata.scss');
    return (
      <div className={metaDataStyles.display_inline}>
        <button
          data-test="data-clear-access-key"
          className={styles.default_button + ' ' + metaDataStyles.margin_right}
          onClick={e => {
            e.preventDefault();
            this.setState({ isClearing: true });
            if (globals.isAccessKeySet || globals.accessKey) {
              clearAccessKeyState();
              console.log('Clearing access key');
              this.props.router.push('/login');
            } else {
              console.log('No access key set');
              showErrorNotification('No access key set');
            }
            this.props.dispatch(showSuccessNotification('Cleared Access Key'));
            this.setState({ isClearing: false });
          }}
        >
          {this.state.isClearing ? 'Clearing...' : 'Clear access key (logout)'}
        </button>
      </div>
    );
  }
}

ClearAccessKey.propTypes = {
  dispatch: PropTypes.func.isRequired,
  dataHeaders: PropTypes.object.isRequired,
};

export default ClearAccessKey;
