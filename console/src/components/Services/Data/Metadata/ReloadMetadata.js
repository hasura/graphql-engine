import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';

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
          className={this.props.bsClass || styles.default_button}
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
                ...this.props.dataHeaders,
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
          {this.state.isReloading
            ? this.props.btnTextChanging || 'Reloading...'
            : this.props.btnText || 'Reload'}
        </button>
      </div>
    );
  }
}

ReloadMetadata.propTypes = {
  dispatch: PropTypes.func.isRequired,
  dataHeaders: PropTypes.object.isRequired,
};

export default ReloadMetadata;
