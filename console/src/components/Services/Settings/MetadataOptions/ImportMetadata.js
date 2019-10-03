import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Button from '../../../Common/Button/Button';

import { replaceMetadataFile } from '../Actions';

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
    const successCb = () => {
      this.setState({ isImporting: false });
    };

    const errorCb = () => {
      this.setState({ isImporting: false });
    };

    this.props.dispatch(replaceMetadataFile(fileContent, successCb, errorCb));
  }
  render() {
    const metaDataStyles = require('../Settings.scss');
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
