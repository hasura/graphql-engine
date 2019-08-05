import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import Button from '../../../Common/Button/Button';

import {
  showSuccessNotification,
  showErrorNotification,
} from '../../Common/Notification';

class ImportMetadata extends Component {
  constructor() {
    super();
    this.state = {};
    this.state.isImporting = false;
  }
  mockFileInput(c) {
    const element = document.createElement('div');
    element.innerHTML = '<input style="display:none" type="file">';
    document.body.appendChild(element);
    const fileInput = element.firstChild;

    fileInput.addEventListener('change', () => {
      c(element, fileInput);
    });

    fileInput.click();
  }
  importMetadata(fileContent) {
    this.setState({ isImporting: true });
    const url = Endpoints.query;
    let requestBody = {};
    try {
      const jsonContent = JSON.parse(fileContent);
      requestBody = {
        type: 'replace_metadata',
        args: jsonContent,
      };
    } catch (e) {
      alert('Error parsing JSON' + e.toString());
      this.setState({ isImporting: false });
      return;
    }
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
            this.setState({ isImporting: false });
            this.props.dispatch(
              showSuccessNotification('Metadata imported successfully!')
            );
          } else {
            const parsedErrorMsg = data;
            this.props.dispatch(
              showErrorNotification(
                'Metadata import failed',
                'Something is wrong.',
                parsedErrorMsg
              )
            );
            this.setState({ isImporting: false });
            console.error('Error with response', parsedErrorMsg);
          }
        });
      })
      .catch(error => {
        console.error(error);
        this.props.dispatch(
          showErrorNotification(
            'Metadata import failed',
            'Cannot connect to server'
          )
        );
        this.setState({ isImporting: false });
      });
  }
  render() {
    const metaDataStyles = require('../Metadata.scss');
    return (
      <div className={metaDataStyles.display_inline}>
        <Button
          data-test="data-import-metadata"
          size="sm"
          color="white"
          onClick={e => {
            e.preventDefault();
            const currThis = this;
            const processInputFile = (nodeElem, fileInput) => {
              const file = fileInput.files[0];
              if (file.name.match(/\.(json)$/)) {
                const reader = new FileReader();
                // Will execute this function once the reader has successfully read the file
                reader.onload = () => {
                  nodeElem.remove();
                  currThis.importMetadata(reader.result);
                };
                reader.readAsText(file);
              } else {
                alert('Please upload a .json file only.');
                nodeElem.remove();
              }
            };
            this.mockFileInput(processInputFile);
          }}
        >
          {this.state.isImporting ? 'Importing...' : 'Import metadata'}
        </Button>
      </div>
    );
  }
}

ImportMetadata.propTypes = {
  dispatch: PropTypes.func.isRequired,
  dataHeaders: PropTypes.object.isRequired,
};

export default ImportMetadata;
