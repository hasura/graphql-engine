import { Card, Collapsible, InputField, Text } from '@hasura/shared/ui';
import { GraphQLCustomization } from '../../GraphQLCustomization';
import { LimitedFeatureWrapper } from '../../LimitedFeatureWrapper/LimitedFeatureWrapper';
import { DatabaseUrl } from './DatabaseUrl';
import { ExtensionSchema } from './ExtensionSchema';
import { IsolationLevel } from './IsolationLevel';
import { PoolSettings } from './PoolSettings';
import { ReadReplicas } from './ReadReplicas';
import { SslSettings } from './SslSettings';
import { UsePreparedStatements } from './UsePreparedStatements';

export const ConnectPostgresForm = ({
  hiddenOptions,
}: {
  hiddenOptions: string[];
}) => {
  return (
    <>
      <InputField
        name="name"
        label="Database name"
        fieldProps={{
          placeholder: 'Database name',
        }}
      />

      <Card size="2">
        <DatabaseUrl
          name="configuration.connectionInfo.databaseUrl"
          hideOptions={hiddenOptions}
        />
      </Card>

      <div className="mt-2">
        <Collapsible
          triggerChildren={
            <Text className="cursor-pointer" weight="bold">
              Advanced Settings
            </Text>
          }
        >
          <PoolSettings name={`configuration.connectionInfo.poolSettings`} />
          <div className="mb-4">
            <IsolationLevel
              name={`configuration.connectionInfo.isolationLevel`}
            />
          </div>
          <UsePreparedStatements
            name={`configuration.connectionInfo.usePreparedStatements`}
          />
          <ExtensionSchema name="configuration.extensionSchema" />
          <LimitedFeatureWrapper
            title="Looking to add SSL Settings?"
            id="db-ssl-settings"
            description="Get production-ready today with a 30-day free trial of Hasura EE, no credit card required."
          >
            <div className="mt-2">
              <SslSettings name={`configuration.connectionInfo.sslSettings`} />
            </div>
          </LimitedFeatureWrapper>
        </Collapsible>
      </div>
      <div className="mt-2">
        <Collapsible
          triggerChildren={
            <Text as="span" weight="bold" className="cursor-pointer">
              GraphQL Customization
            </Text>
          }
        >
          <GraphQLCustomization name="customization" />
        </Collapsible>
      </div>

      <div className="mt-2">
        <LimitedFeatureWrapper
          id="read-replicas"
          title="Improve performance and handle increased traffic with read replicas"
          description="Scale your database by offloading read queries to
read-only replicas, allowing for better performance
and availability for users."
        >
          <Collapsible
            triggerChildren={
              <Text as="span" weight="bold" className="cursor-pointer">
                Read Replicas
              </Text>
            }
          >
            <ReadReplicas
              name="configuration.readReplicas"
              hideOptions={hiddenOptions}
            />
          </Collapsible>
        </LimitedFeatureWrapper>
      </div>
    </>
  );
};
