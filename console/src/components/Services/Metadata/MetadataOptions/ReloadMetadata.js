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

    this.state = {
      isReloading: false,
    };
  }

  render() {
    const { dispatch } = this.props;
    const { isReloading } = this.state;

    const metaDataStyles = require('../Metadata.scss');

    const reloadMetadataAndLoadInconsistentMetadata = e => {
      e.preventDefault();

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
          disabled={this.state.isReloading}
          onClick={reloadMetadataAndLoadInconsistentMetadata}
        >
          {this.props.buttonText || buttonText}
        </Button>
      </div>
    );
  }
}

ReloadMetadata.propTypes = {
  dispatch: PropTypes.func.isRequired,
  buttonText: PropTypes.string,
};

export default ReloadMetadata;
