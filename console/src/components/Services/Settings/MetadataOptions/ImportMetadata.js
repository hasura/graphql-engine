import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Button from '../../../Common/Button/Button';

import { uploadFile } from '../../../Common/utils/jsUtils';
import { replaceMetadataFromFile } from '../../../../metadata/actions';

class ImportMetadata extends Component {
  constructor() {
    super();

    this.state = {
      isImporting: false,
    };

    this.importMetadata = this.importMetadata.bind(this);
  }

  importMetadata(fileContent) {
    const { dispatch } = this.props;

    const successCb = () => {
      this.setState({ isImporting: false });
    };

    const errorCb = () => {
      this.setState({ isImporting: false });
    };

    this.setState({ isImporting: true });

    dispatch(replaceMetadataFromFile(fileContent, successCb, errorCb));
  }

  render() {
    const styles = require('../Settings.scss');

    const { isImporting } = this.state;

    const handleImport = e => {
      e.preventDefault();

      uploadFile(this.importMetadata, 'json');
    };

    return (
      <div className={styles.display_inline}>
        <Button
          data-test="data-import-metadata"
          size="sm"
          color="white"
          onClick={handleImport}
        >
          {isImporting ? 'Importing...' : 'Import metadata'}
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
