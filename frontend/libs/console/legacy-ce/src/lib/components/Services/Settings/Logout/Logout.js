import React from 'react';
import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';
import ClearAdminSecret from './ClearAdminSecret';

const Logout = props => {
  return (
    <Analytics name="Logout" {...REDACT_EVERYTHING}>
      <div className="clear-both pl-md pt-md mb-md bootstrap-jail">
        <h2 className="text-xl font-bold mb-sm">Logout (clear admin-secret)</h2>

        <div>
          <div key="access_key_reset_1" className="mb-sm">
            <div className="w-8/12">
              The console caches the admin-secret (HASURA_GRAPHQL_ADMIN_SECRET)
              in the browser. You can clear this cache to force a prompt for the
              admin-secret when the console is accessed next using this browser.
            </div>
          </div>

          <div key="access_key_reset_2">
            <ClearAdminSecret {...props} />
          </div>
        </div>
      </div>
    </Analytics>
  );
};

const mapStateToProps = state => {
  return {
    ...state.main,
    metadata: state.metadata,
    dataHeaders: { ...state.tables.dataHeaders },
  };
};

const logoutConnector = connect => connect(mapStateToProps)(Logout);

export default logoutConnector;
