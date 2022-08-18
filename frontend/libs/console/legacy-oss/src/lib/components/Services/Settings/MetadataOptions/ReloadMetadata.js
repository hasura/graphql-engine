import React, { Component } from 'react';
import PropTypes from 'prop-types';

import Button from '../../../Common/Button/Button';
import {
  showSuccessNotification,
  showErrorNotification,
} from '../../Common/Notification';
import Tooltip from '../../../Common/Tooltip/Tooltip';
import { reloadMetadata } from '../../../../metadata/actions';
import { focusYellowRing } from '../../Data/constants';

class ReloadMetadata extends Component {
  constructor(props) {
    super(props);
    this.state = {
      isReloading: false,
      shouldReloadRemoteSchemas: false,
      shouldReloadAllSources: false,
    };
  }

  toggleShouldReloadRemoteSchemas = () => {
    this.setState(state => ({
      shouldReloadRemoteSchemas: !state.shouldReloadRemoteSchemas,
    }));
  };

  toggleShouldReloadAllSources = () => {
    this.setState(state => ({
      shouldReloadAllSources: !state.shouldReloadAllSources,
    }));
  };

  render() {
    const {
      dispatch,
      btnTooltipMessage,
      tooltipStyle,
      showReloadRemoteSchemas = true,
    } = this.props;
    const { isReloading, shouldReloadRemoteSchemas, shouldReloadAllSources } =
      this.state;

    const reloadMetadataAndLoadInconsistentMetadata = e => {
      e.preventDefault();

      this.setState({ isReloading: true });

      dispatch(
        reloadMetadata(
          shouldReloadRemoteSchemas,
          shouldReloadAllSources,
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
          className="mr-sm"
        >
          {this.props.buttonText || buttonText}
        </Button>
        {btnTooltipMessage && (
          <Tooltip message={btnTooltipMessage} tooltipStyle={tooltipStyle} />
        )}
        {showReloadRemoteSchemas && (
          <div className="items-center flex">
            <label
              className="cursor-pointer mr-xs ml-md mb-0"
              disabled={this.state.isReloading}
            >
              <input
                type="checkbox"
                onChange={this.toggleShouldReloadRemoteSchemas}
                checked={shouldReloadRemoteSchemas}
                readOnly
                className={`mr-xs cursor-pointer ${focusYellowRing} rounded-sm !mr-xs`}
              />
              Reload all remote schemas
            </label>
            <Tooltip message="Check this if you have inconsistent remote schemas or if your remote schema has changed." />
          </div>
        )}
        <div className="items-center flex">
          <label
            className="cursor-pointer mr-xs ml-md mb-0"
            disabled={this.state.isReloading}
          >
            <input
              type="checkbox"
              onChange={this.toggleShouldReloadAllSources}
              checked={shouldReloadAllSources}
              readOnly
              className={`mr-xs cursor-pointer ${focusYellowRing} rounded-sm !mr-xs`}
            />
            Reload all databases
          </label>
          <Tooltip message="Check this if you have inconsistent databases." />
        </div>
      </>
    );
  }
}

ReloadMetadata.propTypes = {
  dispatch: PropTypes.func.isRequired,
  buttonText: PropTypes.string,
};

export default ReloadMetadata;
