import React, { useEffect } from 'react';
import Common from '../Common/Common';

import { addRemoteSchema, RESET } from './addRemoteSchemaReducer';
import Helmet from 'react-helmet';
import Button from '../../../Common/Button/Button';
import { RemoteSchema } from '@/features/RemoteSchema';
import { appPrefix, pageTitle } from '../constants';
import {
  availableFeatureFlagIds,
  FeatureFlagToast,
  useIsFeatureFlagEnabled,
} from '@/features/FeatureFlags';
import { exportMetadata } from '@/metadata/actions';
import _push from '../../Data/push';

const Add = ({ isRequesting, dispatch, ...props }) => {
  const styles = require('../RemoteSchema.scss');

  useEffect(() => {
    return () => {
      dispatch({ type: RESET });
    };
  }, []);

  const { isLoading, enabled } = useIsFeatureFlagEnabled(
    availableFeatureFlagIds.addRemoteSchemaId
  );

  if (isLoading) {
    return 'Loading...';
  }

  if (enabled) {
    return (
      <RemoteSchema.Create
        onSuccess={remoteSchemaName => {
          // This only exists right now because the sidebar is reading from redux state
          dispatch(exportMetadata()).then(() => {
            dispatch(_push(`${appPrefix}/manage/${remoteSchemaName}/details`));
          });
        }}
      />
    );
  }

  return (
    <div className={styles.addWrapper}>
      <Helmet title={`Add ${pageTitle} - ${pageTitle}s | Hasura`} />
      <div className={styles.heading_text}>Add a new remote schema</div>
      <form
        onSubmit={e => {
          e.preventDefault();
          dispatch(addRemoteSchema());
        }}
      >
        <Common isNew {...props} dispatch={dispatch} />
        <div className={styles.commonBtn}>
          <Button
            type="submit"
            color="yellow"
            size="sm"
            data-test="add-remote-schema-submit"
          >
            {isRequesting ? 'Adding...' : 'Add Remote Schema'}
          </Button>
        </div>
      </form>
      <FeatureFlagToast flagId={availableFeatureFlagIds.addRemoteSchemaId} />
    </div>
  );
};

const mapStateToProps = state => {
  return {
    ...state.remoteSchemas.addData,
    ...state.remoteSchemas.headerData,
  };
};

export default connect => connect(mapStateToProps)(Add);
