import {
  REQUEST_SUCCESS,
  updateSchemaInfo,
} from '@/components/Services/Data/DataActions';
import {
  allowedMetadataTypes,
  useMetadata,
  useMetadataMigration,
} from '@/features/MetadataAPI';
import { useDispatch } from 'react-redux';
import { AnyAction } from 'redux';
import { ReduxState } from '@/types';
import { ThunkDispatch } from 'redux-thunk';
import { useFireNotification } from '@/new-components/Notifications';

export const useSetRoleToAllowListPermission = (collectionName: string) => {
  const { fireNotification } = useFireNotification();
  const { data: metadata } = useMetadata();
  const dispatch: ThunkDispatch<ReduxState, unknown, AnyAction> = useDispatch();

  // to check if collection is present in allowList
  const isAllowList = metadata?.metadata.allowlist?.find(
    qs => qs.collection === collectionName
  );

  const mutation = useMetadataMigration({
    onSuccess: () => {
      dispatch({ type: REQUEST_SUCCESS });
      dispatch(updateSchemaInfo()).then(() => {
        fireNotification({
          title: 'Success!',
          message: 'Permission added',
          type: 'success',
        });
      });
    },
    onError: (error: Error) => {
      fireNotification({
        title: 'Error',
        message: error?.message ?? 'Error while adding permission',
        type: 'error',
      });
    },
  });

  const setRoleToAllowListPermission = (roles: string[]): void => {
    // drop collection from allow list if we are disabling the last role
    if (roles.length === 0) {
      const type: allowedMetadataTypes = 'drop_collection_from_allowlist';
      mutation.mutate({
        query: {
          type,
          args: {
            collection: collectionName,
          },
        },
      });
    } else if (!isAllowList) {
      // add collection to allow list and add roles if we are enabling the 1st role
      const type: allowedMetadataTypes = 'add_collection_to_allowlist';
      mutation.mutate({
        query: {
          type,
          args: {
            collection: collectionName,
            scope: {
              global: false,
              roles,
            },
          },
        },
      });
    } else {
      const type: allowedMetadataTypes =
        'update_scope_of_collection_in_allowlist';
      mutation.mutate({
        query: {
          type,
          args: {
            collection: collectionName,
            scope: {
              global: false,
              roles,
            },
          },
        },
      });
    }
  };

  return { setRoleToAllowListPermission };
};
