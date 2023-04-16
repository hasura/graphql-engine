import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { Button } from '../../../../new-components/Button';

import {
  showSuccessNotification,
  showErrorNotification,
} from '../../Common/Notification';
import {
  downloadObjectAsJsonFile,
  getCurrTimeForFileName,
} from '../../../Common/utils/export.utils';
import { exportMetadata } from '../../../../metadata/actions';

class ExportMetadata extends Component {
  constructor() {
    super();

    this.state = {
      isExporting: false,
    };
  }

  render() {
    const { isExporting } = this.state;

    const { dispatch } = this.props;

    const handleExport = e => {
      e.preventDefault();

      this.setState({ isExporting: true });

      const successCallback = data => {
        const fileName =
          'hasura_metadata_' + getCurrTimeForFileName() + '.json';

        downloadObjectAsJsonFile(fileName, data);

        this.setState({ isExporting: false });

        dispatch(
          showSuccessNotification(
            'Metadata exported successfully!',
            `Metadata file "${fileName}"`
          )
        );
      };

      const errorCallback = error => {
        this.setState({ isExporting: false });

        dispatch(showErrorNotification('Metadata export failed', null, error));
      };

      dispatch(exportMetadata(successCallback, errorCallback));
    };

    return (
      <div className="inline-block">
        <Button
          data-testid="data-export-metadata"
          className="mr-md"
          size="sm"
          isLoading={isExporting}
          loadingText="Exporting..."
          onClick={handleExport}
        >
          Export metadata
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
