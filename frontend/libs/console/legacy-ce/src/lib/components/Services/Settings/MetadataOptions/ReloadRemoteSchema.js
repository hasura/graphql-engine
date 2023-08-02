import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { Button } from '../../../../new-components/Button';

import {
  showSuccessNotification,
  showErrorNotification,
} from '../../Common/Notification';
import { reloadRemoteSchema } from '../../../../metadata/actions';

class ReloadRemoteSchema extends Component {
  constructor() {
    super();
    this.state = {};
    this.state.isReloading = false;
  }
  render() {
    const { dispatch, remoteSchemaName } = this.props;
    const { isReloading } = this.state;
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

    return (
      <div className="inline-block">
        <Button
          isLoading={isReloading}
          data-test="data-reload-metadata"
          onClick={reloadRemoteMetadataHandler}
        >
          Reload
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
