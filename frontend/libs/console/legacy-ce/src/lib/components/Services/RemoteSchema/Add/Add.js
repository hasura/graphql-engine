import React, { useEffect } from 'react';

import { RESET } from './addRemoteSchemaReducer';
import { RemoteSchema } from '../../../../features/RemoteSchema';
import { appPrefix } from '../constants';
import { exportMetadata } from '../../../../metadata/actions';
import _push from '../../Data/push';

const Add = ({ isRequesting, dispatch, ...props }) => {
  useEffect(() => {
    return () => {
      dispatch({ type: RESET });
    };
  }, []);

  return (
    <RemoteSchema.Create
      {...props}
      onSuccess={remoteSchemaName => {
        // This only exists right now because the sidebar is reading from redux state
        dispatch(exportMetadata()).then(() => {
          dispatch(_push(`${appPrefix}/manage/${remoteSchemaName}/details`));
        });
      }}
    />
  );
};

const mapStateToProps = state => {
  return {
    ...state.remoteSchemas.addData,
    ...state.remoteSchemas.headerData,
  };
};

export default connect => connect(mapStateToProps)(Add);
