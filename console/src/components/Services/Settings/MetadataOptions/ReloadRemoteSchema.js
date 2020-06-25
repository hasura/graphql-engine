import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Button from '../../../Common/Button/Button';
import { reloadRemoteSchema } from '../Actions';
import metaDataStyles from '../Settings.scss';

import {
  showSuccessNotification,
  showErrorNotification,
} from '../../Common/Notification';
// import { reloadMetadata } from '../Actions';

class ReloadRemoteSchema extends Component {
  constructor() {
    super();
    this.state = {};
    this.state.isReloading = false;
  }
  render() {
    const { dispatch } = this.props;
    console.group(this.props);
    const { isReloading } = this.state;
    const reloadRemoteMetadataHandler = () => {
      this.setState({ isReloading: true });
      dispatch(
        // TODO: if the remote schema is within inconsistent objects, it breaks the app
        reloadRemoteSchema(
          this.props.remoteSchemaName,
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
        // reloadMetadata(
        //   true,
        //   () => {
        //     showSuccessNotification('Remote schema reloaded');
        //     this.setState({ isReloading: false });
        //   },
        //   () => {
        //     showErrorNotification('Error reloading remote schema', null);
        //   }
        // )
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
