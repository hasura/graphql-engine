import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Button from '../../../Common/Button/Button';
import { reloadMetadata } from '../Actions';

import {
  showSuccessNotification,
  showErrorNotification,
} from '../../Common/Notification';

class ReloadMetadata extends Component {
  constructor() {
    super();
    this.state = {};
    this.state.isReloading = false;
  }
  render() {
    const { dispatch } = this.props;
    const { isReloading } = this.state;
    const metaDataStyles = require('../Metadata.scss');
    const reloadMetadataAndLoadInconsistentMetadata = () => {
      this.setState({ isReloading: true });
      dispatch(
        reloadMetadata(
          () => {
            dispatch(showSuccessNotification('Metadata reloaded'));
            this.setState({ isReloading: false });
          },
          err => {
            dispatch(
              showErrorNotification('Error reloading metadata', null, err)
            );
            this.setState({ isReloading: false });
          }
        )
      );
    };
    const buttonText = isReloading ? 'Reloading' : 'Reload';
    return (
      <div className={metaDataStyles.display_inline}>
        <Button
          data-test="data-reload-metadata"
          color="white"
          size="sm"
          onClick={reloadMetadataAndLoadInconsistentMetadata}
        >
          {buttonText}
        </Button>
      </div>
    );
  }
}

ReloadMetadata.propTypes = {
  dispatch: PropTypes.func.isRequired,
  dataHeaders: PropTypes.object.isRequired,
};

export default ReloadMetadata;
