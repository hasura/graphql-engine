import { Nullable } from './../../../Common/utils/tsUtils';
import { exportMetadata, MetadataActions } from '../../../../metadata/actions';
import {
  APILimitInputType,
  removeAPILimitsQuery,
  updateAPILimitsQuery,
  updateIntrospectionOptionsQuery,
} from '../../../../metadata/utils';
import { Thunk } from '../../../../types';
import { makeMigrationCall } from '../../Data/DataActions';
import { hasuraToast } from '../../../../new-components/Toasts';

export type updateSecurityFeaturesActionType = {
  api_limits: {
    disabled: boolean;
    depth_limit?: APILimitInputType<number>;
    batch_limit?: APILimitInputType<number>;
    node_limit?: APILimitInputType<number>;
    time_limit?: APILimitInputType<number>;
    rate_limit?: APILimitInputType<{
      unique_params: Nullable<'IP' | string[]>;
      max_reqs_per_min: number;
    }>;
  };

  successCb?: () => void;
  errorCb?: () => void;
};
export const updateAPILimits = ({
  api_limits,
  successCb,
  errorCb,
}: updateSecurityFeaturesActionType): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const existingAPILimits = getState().metadata?.metadataObject?.api_limits;
    const upQuery = updateAPILimitsQuery({
      existingAPILimits,
      newAPILimits: api_limits,
    });

    const migrationName = `set_api_limits`;
    const requestMsg = 'Updating API limits...';
    const successMsg = 'Updated API limits';
    const errorMsg = 'Updating API limits failed';

    const onSuccess = (
      response: [{ warnings: { code: string; message: string }[] }]
    ) => {
      dispatch(exportMetadata());
      successCb?.();
      response?.[0]?.warnings?.forEach(i => {
        hasuraToast({
          type: 'warning',
          title: 'Time Limit Exceeded System Limit',
          message: i.message,
          toastOptions: {
            duration: Infinity,
          },
        });
      });
    };

    const onError = () => {
      errorCb?.();
    };

    makeMigrationCall(
      dispatch,
      getState,
      [upQuery],
      undefined,
      migrationName,
      onSuccess,
      onError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

export const removeAPILimits = ({
  role,
  callback,
}: {
  role: string;
  callback: () => void;
}): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const existingAPILimits = getState().metadata?.metadataObject?.api_limits;

    const upQuery = removeAPILimitsQuery({
      existingAPILimits,
      role,
    });

    const migrationName = `set_api_limits`;
    const requestMsg = 'Removing API limits...';
    const successMsg = 'Removing API limits';
    const errorMsg = 'Removing API limits failed';

    const onSuccess = (response: [{ warnings: [] }]) => {
      dispatch(exportMetadata());
      if (callback) {
        callback();
      }
      response?.[0]?.warnings?.forEach(
        (i: { code: string; message: string }) => {
          hasuraToast({
            type: 'warning',
            title: 'Time Limit Exceeded System Limit',
            message: i.message,
            toastOptions: {
              duration: Infinity,
            },
          });
        }
      );
    };

    const onError = () => {};

    makeMigrationCall(
      dispatch,
      getState,
      [upQuery],
      undefined,
      migrationName,
      onSuccess,
      onError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

export type updateIntrospectionOptionsActionType = {
  introspectionIsDisabled: boolean;
  roleName: string;
  callback?: () => void;
};
export const updateIntrospectionOptions = ({
  introspectionIsDisabled,
  roleName,
  callback,
}: updateIntrospectionOptionsActionType): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const existingOptions =
      getState().metadata?.metadataObject?.graphql_schema_introspection
        ?.disabled_for_roles ?? [];

    const upQuery = updateIntrospectionOptionsQuery({
      existingOptions,
      introspectionIsDisabled,
      roleName,
    });
    const migrationName = `set_graphql_introspection_options`;
    const requestMsg = 'Updating Introspection options...';
    const successMsg = 'Updated Introspection!';
    const errorMsg = 'Updating Introspection options failed';

    const onSuccess = () => {
      dispatch(exportMetadata());
      if (callback) {
        callback();
      }
    };

    const onError = () => {};

    makeMigrationCall(
      dispatch,
      getState,
      [upQuery],
      undefined,
      migrationName,
      onSuccess,
      onError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};
