import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { Button } from '../../../../new-components/Button';

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
    const { isImporting } = this.state;

    const handleImport = e => {
      e.preventDefault();

      uploadFile(this.importMetadata, 'json');
    };

    return (
      <div className="inline-block">
        <Button
          data-test="data-import-metadata"
          size="sm"
          isLoading={isImporting}
          loadingText="Importing..."
          onClick={handleImport}
        >
          Import metadata
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
