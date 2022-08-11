import React, { Component } from 'react';
import { Connect } from 'react-redux';
import Helmet from 'react-helmet';
import { FaSpinner } from 'react-icons/fa';

import globals from '../../../../Globals';

import { ReduxState, ConnectInjectedProps } from '../../../../types';

type AboutState = {
  consoleAssetVersion?: string;
};

class About extends Component<ConnectInjectedProps & StateProps> {
  // had to add this here as the state type is not being read properly if added above.
  override state: AboutState = {
    consoleAssetVersion: globals.consoleAssetVersion,
  };

  override render() {
    const { consoleAssetVersion } = this.state;

    const { serverVersion } = this.props;

    const spinner = <FaSpinner className="animate-spin" />;

    const getServerVersionSection = () => {
      return (
        <div>
          <b>Current server version: </b>
          <span className="ml-sm font-light">{serverVersion || spinner}</span>
        </div>
      );
    };

    const getConsoleAssetVersionSection = () => {
      return (
        <div>
          <b>Console asset version: </b>
          <span className="ml-sm font-light">
            {consoleAssetVersion || 'NA'}
          </span>
        </div>
      );
    };

    return (
      <div className="clear-both pl-sm pt-md mb-sm">
        <div className="text-base font-bold">
          <Helmet title="About | Hasura" />
          <h2 className="text-xl font-bold">About</h2>
          <div className="mt-sm">{getServerVersionSection()}</div>
          <div className="mt-sm">{getConsoleAssetVersionSection()}</div>
        </div>
      </div>
    );
  }
}

const mapStateToProps = (state: ReduxState) => {
  return {
    dataHeaders: state.tables.dataHeaders,
    serverVersion: state.main.serverVersion,
    source: state.tables.currentDataSource,
    latestStableServerVersion: state.main.latestStableServerVersion,
  };
};

type StateProps = ReturnType<typeof mapStateToProps>;

const aboutConnector = (connect: Connect) => connect(mapStateToProps)(About);

export default aboutConnector;
