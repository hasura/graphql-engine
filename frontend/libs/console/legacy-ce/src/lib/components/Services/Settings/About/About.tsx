import React from 'react';
import { Connect } from 'react-redux';
import Helmet from 'react-helmet';
import { FaSpinner } from 'react-icons/fa';

import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';
import { EELicenseInfo } from './EELicenseInfo';
import { LabelValue } from './LabelValue';
import globals from '../../../../Globals';
import { ReduxState } from '../../../../types';

export const About: React.VFC<StateProps> = props => {
  const { serverVersion, consoleAssetVersion } = props;

  const spinner = <FaSpinner className="animate-spin" />;

  return (
    <Analytics name="About" {...REDACT_EVERYTHING}>
      <div className="clear-both pl-md pt-md mb-sm bootstrap-jail">
        <div className="text-base font-bold">
          <Helmet title="About | Hasura" />
          <h2 className="text-xl font-bold mb-md">About</h2>
          <div className="mb-md">
            <LabelValue
              label={'Current Server Version'}
              value={serverVersion || spinner}
            />
          </div>
          <div className="mb-md">
            <LabelValue
              label={'Console asset version'}
              value={consoleAssetVersion || 'NA'}
            />
          </div>
          <EELicenseInfo className="mb-md" />
        </div>
      </div>
    </Analytics>
  );
};

const mapStateToProps = (state: ReduxState) => {
  return {
    serverVersion: state.main.serverVersion,
    consoleAssetVersion: globals.consoleAssetVersion,
  };
};

type StateProps = ReturnType<typeof mapStateToProps>;

const aboutConnector = (connect: Connect) => connect(mapStateToProps)(About);

export default aboutConnector;
