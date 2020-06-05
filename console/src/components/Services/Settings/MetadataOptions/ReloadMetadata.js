import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Button from '../../../Common/Button/Button';
import { reloadMetadata } from '../Actions';

import {
  showSuccessNotification,
  showErrorNotification,
} from '../../Common/Notification';
import Tooltip from '../../../Common/Tooltip/Tooltip';
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
    const {
      dispatch,
      buttonToolTip,
      tooltipStyle,
      checkBoxVisible = true,
    } = this.props;
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
      <>
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
        {buttonToolTip && (
          <Tooltip
            message={buttonToolTip}
            icon="fa-info-circle"
            tooltipStyle={tooltipStyle}
          />
        )}
        {checkBoxVisible && (
          <>
            <label
              className={`${metaDataStyles.cursorPointer} ${metaDataStyles.add_mar_right_small} ${metaDataStyles.add_mar_left_small}`}
              disabled={this.state.isReloading}
            >
              <input
                type="checkbox"
                onChange={this.toggleShouldReloadRemoteSchemas}
                checked={shouldReloadRemoteSchemas}
                readOnly
                className={`${metaDataStyles.add_mar_right_small} ${metaDataStyles.cursorPointer}`}
              />
              Reload all remote schemas
            </label>
            <Tooltip message="Check this if you have inconsistent remote schemas or if your remote schema has changed." />
          </>
        )}
      </>
    );
  }
}

ReloadMetadata.propTypes = {
  dispatch: PropTypes.func.isRequired,
  buttonText: PropTypes.string,
};

export default ReloadMetadata;
