import { useState } from 'react';
import { Button, Card, Input, Text } from '@hasura/shared/ui';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { InconsistentBadge } from '../InconsistentBadge';
import { RemoteSchemaDetailsHeaders } from './RemoteSchemaDetailsHeaders';
import { RemoteSchemaDetailsNavigation } from './RemoteSchemaDetailsNavigation';
import { SchemaPreview } from './SchemaPreview';
import {
  useInconsistentMetadata,
  useReloadRemoteSchema,
} from '@hasura/metadata/api';
import { useCurrentRemoteSchemaContext } from '../../context';
import { findInconsistentRemoteSchema } from '@hasura/metadata/helpers';
import { useAppContext } from '@hasura/shared/context';
import { Flex } from '@radix-ui/themes';

const RemoteSchemaDetails = () => {
  const { readOnlyMode } = useAppContext();
  const { data: inconsistentMetadata } = useInconsistentMetadata();
  const { currentRemoteSchema } = useCurrentRemoteSchemaContext();

  const [reloading, setReloading] = useState(false);
  const reloadRemoteSchema = useReloadRemoteSchema();

  const manualUrl =
    currentRemoteSchema.definition && 'url' in currentRemoteSchema.definition
      ? currentRemoteSchema.definition.url
      : undefined;
  const envName =
    currentRemoteSchema.definition &&
    'url_from_env' in currentRemoteSchema.definition
      ? currentRemoteSchema.definition.url_from_env
      : undefined;
  const headers = currentRemoteSchema.definition.headers;
  const introspectionHeaders =
    currentRemoteSchema.definition.introspection_headers;
  // Distinguish inherited (property absent) from an explicit empty list.
  const hasIntrospectionHeaders = Array.isArray(introspectionHeaders);

  const inconsistencyDetails = findInconsistentRemoteSchema(
    inconsistentMetadata?.inconsistent_objects,
    currentRemoteSchema.name,
  );

  const reload = () => {
    setReloading(true);
    reloadRemoteSchema(currentRemoteSchema.name).finally(() => {
      setReloading(false);
    });
  };

  return (
    <Analytics name="RemoteSchemaDetails" {...REDACT_EVERYTHING}>
      <div className="px-6">
        <RemoteSchemaDetailsNavigation
          remoteSchemaName={currentRemoteSchema.name}
        />
        {inconsistencyDetails && (
          <InconsistentBadge inconsistencyDetails={inconsistencyDetails} />
        )}
        <div className="w-full sm:w-9/12">
          <div className="mb-4">
            <Card className="w-full show">
              <div className="mb-4">
                <Text weight="bold">Server GraphQL URL</Text>
                <Flex align="center" className="mt-2">
                  <Input
                    type="text"
                    placeholder={manualUrl || `<${envName}>`}
                    full
                    disabled
                    rightButton={
                      !readOnlyMode ? (
                        <Button
                          mode="default"
                          onClick={reload}
                          loading={reloading}
                        >
                          Reload
                        </Button>
                      ) : undefined
                    }
                  />
                </Flex>
              </div>
              <RemoteSchemaDetailsHeaders
                headers={headers}
                title="Request headers"
              />
              {!hasIntrospectionHeaders ? (
                <div className="mb-4">
                  <Text weight="bold" as="p">
                    Introspection headers
                  </Text>
                  <Text>Inherited from request headers</Text>
                </div>
              ) : introspectionHeaders?.length === 0 ? (
                <div className="mb-4">
                  <Text weight="bold" as="p">
                    Introspection headers
                  </Text>
                  <Text>None</Text>
                </div>
              ) : (
                <RemoteSchemaDetailsHeaders
                  headers={introspectionHeaders}
                  title="Introspection headers"
                />
              )}
              <Text weight="bold" as="p">
                Remote Schema Preview
              </Text>
              <Card className="mt-2">
                <SchemaPreview name={currentRemoteSchema.name} />
              </Card>
            </Card>
          </div>
        </div>
      </div>
    </Analytics>
  );
};

export default RemoteSchemaDetails;
