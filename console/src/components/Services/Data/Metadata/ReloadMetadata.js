import React, { Component } from 'react';
import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import globals from '../../../../Globals';

import {
  showSuccessNotification,
  showErrorNotification,
} from '../Notification';

class ReloadMetadata extends Component {
  constructor() {
    super();
    this.state = {};
    this.state.isReloading = false;
  }
  render() {
    const styles = require('../PageContainer/PageContainer.scss');
    const metaDataStyles = require('./Metadata.scss');
    return (
      <div className={metaDataStyles.display_inline}>
        <button
          data-test="data-reload-metadata"
          className={styles.default_button + ' ' + metaDataStyles.margin_right}
          onClick={e => {
            e.preventDefault();
            this.setState({ isReloading: true });
            const url = Endpoints.query;
            const requestBody = {
              type: 'reload_metadata',
              args: {},
            };
            const options = {
              method: 'POST',
              credentials: globalCookiePolicy,
              headers: {
                'X-Hasura-Access-Key': globals.accessKey,
              },
              body: JSON.stringify(requestBody),
            };
            fetch(url, options)
              .then(response => {
                response.json().then(data => {
                  if (response.ok) {
                    this.setState({ isReloading: false });
                    this.props.dispatch(
                      showSuccessNotification('Metadata reloaded successfully!')
                    );
                  } else {
                    const parsedErrorMsg = data;
                    this.props.dispatch(
                      showErrorNotification(
                        'Metadata reload failed',
                        'Something is wrong.',
                        requestBody,
                        parsedErrorMsg
                      )
                    );
                    console.error('Error with response', parsedErrorMsg);
                    this.setState({ isReloading: false });
                  }
                });
              })
              .catch(error => {
                console.error(error);
                this.props.dispatch(
                  showErrorNotification(
                    'Metadata reload failed',
                    'Cannot connect to server'
                  )
                );
                this.setState({ isReloading: false });
              });
          }}
        >
          {this.state.isReloading ? 'Reloading...' : 'Reload'}
        </button>
      </div>
    );
  }
}

export default ReloadMetadata;
