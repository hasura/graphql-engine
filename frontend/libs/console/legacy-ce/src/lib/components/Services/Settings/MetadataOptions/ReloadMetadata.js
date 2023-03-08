import React, { Component } from 'react';
import PropTypes from 'prop-types';

import { Button } from '../../../../new-components/Button';
import { IconTooltip } from '../../../../new-components/Tooltip';
import {
  showSuccessNotification,
  showErrorNotification,
} from '../../Common/Notification';
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
      <div className="flex items-center gap-6">
        <Button
          data-test="data-reload-metadata"
          size="sm"
          disabled={this.state.isReloading}
          onClick={reloadMetadataAndLoadInconsistentMetadata}
        >
          {this.props.buttonText || buttonText}
        </Button>
        {btnTooltipMessage && <IconTooltip message={btnTooltipMessage} />}
        {showReloadRemoteSchemas && (
          <div className="items-center flex">
            <label
              className="cursor-pointer flex items-center"
              disabled={this.state.isReloading}
            >
              <input
                type="checkbox"
                onChange={this.toggleShouldReloadRemoteSchemas}
                checked={shouldReloadRemoteSchemas}
                readOnly
                className={`cursor-pointer ${focusYellowRing} rounded-sm !mr-1 !mt-0`}
              />
              Reload all remote schemas
            </label>
            <IconTooltip message="Check this if you have inconsistent remote schemas or if your remote schema has changed." />
          </div>
        )}
        <div className="items-center flex">
          <label
            className="cursor-pointer flex items-center"
            disabled={this.state.isReloading}
          >
            <input
              type="checkbox"
              onChange={this.toggleShouldReloadAllSources}
              checked={shouldReloadAllSources}
              readOnly
              className={`cursor-pointer ${focusYellowRing} rounded-sm !mr-1 !mt-0`}
            />
            Reload all databases
          </label>
          <IconTooltip message="Check this if you have inconsistent databases." />
        </div>
      </div>
    );
  }
}

ReloadMetadata.propTypes = {
  dispatch: PropTypes.func.isRequired,
  buttonText: PropTypes.string,
};

export default ReloadMetadata;
