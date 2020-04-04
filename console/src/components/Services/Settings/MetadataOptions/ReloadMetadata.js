import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Button from '../../../Common/Button/Button';
import { reloadMetadata } from '../Actions';

import {
  showSuccessNotification,
  showErrorNotification,
} from '../../Common/Notification';
import { ToolTip } from '../../../UIKit/atoms';
import metaDataStyles from '../Settings.scss';

class ReloadMetadata extends Component {
  constructor(props) {
    super(props);
    this.state = {
      isReloading: false,
      shouldReloadRemoteSchemas: props.shouldReloadRemoteSchemas || false,
    };
  }

  toggleShouldReloadRemoteSchemas = () => {
    this.setState(state => ({
      shouldReloadRemoteSchemas: !state.shouldReloadRemoteSchemas,
    }));
  };

  render() {
    const { dispatch } = this.props;
    const { isReloading, shouldReloadRemoteSchemas } = this.state;

    const reloadMetadataAndLoadInconsistentMetadata = e => {
      e.preventDefault();

      this.setState({ isReloading: true });

      dispatch(
        reloadMetadata(
          shouldReloadRemoteSchemas,
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
      <div className={`${metaDataStyles.display_inline}`}>
        <Button
          data-test="data-reload-metadata"
          color="white"
          size="sm"
          disabled={this.state.isReloading}
          onClick={reloadMetadataAndLoadInconsistentMetadata}
          className={`${metaDataStyles.add_mar_right_mid}`}
        >
          {this.props.buttonText || buttonText}
        </Button>
        <label
          onChange={this.toggleShouldReloadRemoteSchemas}
          className={`${metaDataStyles.cursorPointer} ${metaDataStyles.add_mar_right_small}`}
          disabled={this.state.isReloading}
        >
          <input
            type="checkbox"
            checked={shouldReloadRemoteSchemas}
            readOnly
            className={`${metaDataStyles.add_mar_right_small} ${metaDataStyles.cursorPointer}`}
          />
          Reload all remote schemas
        </label>
        <ToolTip
          message="Check this if you have inconsistent remote schemas or if your remote schema has changed."
          ml="sm"
        />
      </div>
    );
  }
}

ReloadMetadata.propTypes = {
  dispatch: PropTypes.func.isRequired,
  buttonText: PropTypes.string,
};

export default ReloadMetadata;
