import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import Button from '../../../Common/Button/Button';

import {
  showSuccessNotification,
  showErrorNotification,
} from '../../Common/Notification';

class ExportMetadata extends Component {
  constructor() {
    super();
    this.state = {};
    this.state.isExporting = false;
  }
  render() {
    const metaDataStyles = require('../Metadata.scss');
    return (
      <div className={metaDataStyles.display_inline}>
        <Button
          data-test="data-export-metadata"
          className={metaDataStyles.margin_right}
          size="sm"
          color="white"
          onClick={e => {
            e.preventDefault();
            this.setState({ isExporting: true });
            const url = Endpoints.query;
            const requestBody = {
              type: 'export_metadata',
              args: null,
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
                    const dataStr =
                      'data:text/json;charset=utf-8,' +
                      encodeURIComponent(JSON.stringify(data));
                    const anchorElem = document.createElement('a');
                    anchorElem.setAttribute('href', dataStr);
                    anchorElem.setAttribute('download', 'metadata.json');
                    // The following fixes the download issue on firefox
                    document.body.appendChild(anchorElem);
                    anchorElem.click();
                    anchorElem.remove();
                    this.setState({ isExporting: false });
                    this.props.dispatch(
                      showSuccessNotification('Metadata exported successfully!')
                    );
                  } else {
                    const parsedErrorMsg = data;
                    this.props.dispatch(
                      showErrorNotification(
                        'Metadata export failed',
                        'Something is wrong.',
                        parsedErrorMsg
                      )
                    );
                    console.error('Error with response', parsedErrorMsg);
                    this.setState({ isExporting: false });
                  }
                });
              })
              .catch(error => {
                console.error(error);
                this.props.dispatch(
                  showErrorNotification(
                    'Metadata export failed',
                    'Cannot connect to server'
                  )
                );
                this.setState({ isExporting: false });
              });
          }}
        >
          {this.state.isExporting ? 'Exporting...' : 'Export metadata'}
        </Button>
      </div>
    );
  }
}

ExportMetadata.propTypes = {
  dispatch: PropTypes.func.isRequired,
  dataHeaders: PropTypes.object.isRequired,
};

export default ExportMetadata;
