import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Button from '../../../Common/Button/Button';
import { reloadRemoteSchema } from '../Actions';

import {
  showSuccessNotification,
  showErrorNotification,
} from '../../Common/Notification';

class ReloadRemoteSchema extends Component {
  constructor() {
    super();
    this.state = {};
    this.state.isReloading = false;
  }
  render() {
    const { dispatch, remoteSchemaName } = this.props;
    const { isReloading } = this.state;
    const metaDataStyles = require('../Metadata.scss');
    const reloadRemoteMetadataHandler = () => {
      this.setState({ isReloading: true });
      dispatch(
        reloadRemoteSchema(
          remoteSchemaName,
          () => {
            dispatch(showSuccessNotification('Remote schema reloaded'));
            this.setState({ isReloading: false });
          },
          error => {
            dispatch(
              showErrorNotification(
                'Error reloading remote schema',
                null,
                error
              )
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
          onClick={reloadRemoteMetadataHandler}
        >
          {buttonText}
        </Button>
      </div>
    );
  }
}

ReloadRemoteSchema.propTypes = {
  dispatch: PropTypes.func.isRequired,
  dataHeaders: PropTypes.object.isRequired,
  remoteSchemaName: PropTypes.string.isRequired,
};

export default ReloadRemoteSchema;
