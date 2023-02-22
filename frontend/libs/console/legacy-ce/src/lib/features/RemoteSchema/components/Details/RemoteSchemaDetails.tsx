import React from 'react';
import { useReadOnlyMode } from '../../../../hooks';
import {
  MetadataSelector,
  useMetadata,
  useMetadataMigration,
  useInconsistentObject,
} from '../../../MetadataAPI';
import { Button } from '../../../../new-components/Button';
import { useFireNotification } from '../../../../new-components/Notifications';
import { Analytics, REDACT_EVERYTHING } from '../../../Analytics';
import { InconsistentBadge } from '../../../../components/Services/RemoteSchema/Common/GraphQLCustomization/InconsistentBadge';

import { RemoteSchemaDetailsHeaders } from './RemoteSchemaDetailsHeaders';
import { RemoteSchemaDetailsNavigation } from './RemoteSchemaDetailsNavigation';
import { SchemaPreview } from './SchemaPreview';

interface RemoteSchemaDetailsProps {
  params: {
    remoteSchemaName: string;
  };
}

export const RemoteSchemaDetails = (props: RemoteSchemaDetailsProps) => {
  const { remoteSchemaName } = props.params;

  const remoteSchema = useMetadata(
    MetadataSelector.getRemoteSchema(remoteSchemaName)
  );

  const readOnlyModeResponse = useReadOnlyMode();

  const inconsistentObjects = useInconsistentObject();

  const { fireNotification } = useFireNotification();

  const { mutate, isLoading: isReloadLoading } = useMetadataMigration();

  const reload = React.useCallback(() => {
    mutate(
      {
        query: {
          type: 'reload_remote_schema',
          args: {
            name: remoteSchemaName,
          },
        },
      },
      {
        onSuccess: () => {
          fireNotification({
            type: 'success',
            title: 'Remote schema cache reloaded',
            message: `Remote schema cache for ${remoteSchemaName} has been reloaded`,
          });
        },
        onError: e => {
          fireNotification({
            type: 'error',
            title: 'Error reloading remote schema cache',
            message: `Error reloading remote schema cache for ${remoteSchemaName}: ${e.message}`,
          });
        },
      }
    );
  }, [mutate, remoteSchemaName, fireNotification]);

  if (!remoteSchema) {
    return null;
  }

  const manualUrl = remoteSchema.data?.definition.url;
  const envName = remoteSchema.data?.definition.url_from_env;
  const headers = remoteSchema.data?.definition.headers;
  const readOnlyMode = readOnlyModeResponse.data || true;

  const inconsistencyDetails = inconsistentObjects.find(
    inconObj =>
      inconObj.type === 'remote_schema' &&
      inconObj?.name === `remote_schema ${remoteSchemaName}`
  );

  return (
    <Analytics name="RemoteSchemaDetails" {...REDACT_EVERYTHING}>
      <div>
        <RemoteSchemaDetailsNavigation remoteSchemaName={remoteSchemaName} />
        {inconsistencyDetails && (
          <InconsistentBadge inconsistencyDetails={inconsistencyDetails} />
        )}
        <div className="w-full sm:w-9/12 ">
          <div className="mb-md">
            <div className="w-full bg-white shadow-sm rounded p-md border border-gray-300 shadow show">
              <div className="mb-md">
                <label className="block mb-xs font-semibold text-muted">
                  Server GraphQL URL
                </label>
                <div className="flex items-center">
                  <input
                    type="text"
                    className="block w-full mr-2 h-input cursor-not-allowed rounded border bg-gray-200 border-gray-200"
                    placeholder={manualUrl || `<${envName}>`}
                    disabled
                  />
                  {readOnlyMode && (
                    <Button onClick={reload} isLoading={isReloadLoading}>
                      Reload
                    </Button>
                  )}
                </div>
              </div>
              <RemoteSchemaDetailsHeaders headers={headers} />
              <label className="block mb-xs text-muted font-semibold">
                Remote Schema Preview
              </label>
              <div className="rounded bg-gray-50 border border-gray-300 px-md py-sm">
                <SchemaPreview name={remoteSchemaName} />
              </div>
            </div>
          </div>
        </div>
      </div>
    </Analytics>
  );
};
