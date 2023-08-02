import { Collapsible } from '../../../../../new-components/Collapsible';
import { InputField } from '../../../../../new-components/Form';
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
        placeholder="Database name"
      />

      <div className="bg-white border border-hasGray-300 rounded-md shadow-sm overflow-hidden p-4">
        <DatabaseUrl
          name="configuration.connectionInfo.databaseUrl"
          hideOptions={hiddenOptions}
        />
      </div>

      <div className="mt-sm">
        <Collapsible
          triggerChildren={
            <div className="font-semibold text-muted">Advanced Settings</div>
          }
        >
          <PoolSettings name={`configuration.connectionInfo.poolSettings`} />
          <IsolationLevel
            name={`configuration.connectionInfo.isolationLevel`}
          />
          <UsePreparedStatements
            name={`configuration.connectionInfo.usePreparedStatements`}
          />
          <ExtensionSchema name="configuration.extensionSchema" />
          <LimitedFeatureWrapper
            title="Looking to add SSL Settings?"
            id="db-ssl-settings"
            description="Get production-ready today with a 30-day free trial of Hasura EE, no credit card required."
          >
            <div className="mt-sm">
              <Collapsible
                triggerChildren={
                  <div className="font-semibold text-muted">
                    SSL Certificates Settings
                    <span className="px-1.5 italic font-light">
                      (Certificates will be loaded from{' '}
                      <a href="https://hasura.io/docs/latest/graphql/cloud/projects/create.html#existing-database">
                        environment variables
                      </a>
                      )
                    </span>
                  </div>
                }
              >
                <SslSettings
                  name={`configuration.connectionInfo.sslSettings`}
                />
              </Collapsible>
            </div>
          </LimitedFeatureWrapper>
        </Collapsible>
      </div>

      <div className="mt-sm">
        <Collapsible
          triggerChildren={
            <div className="font-semibold text-muted">
              GraphQL Customization
            </div>
          }
        >
          <GraphQLCustomization name="customization" />
        </Collapsible>
      </div>

      <div className="mt-sm">
        <LimitedFeatureWrapper
          id="read-replicas"
          title="Improve performance and handle increased traffic with read replicas"
          description="Scale your database by offloading read queries to
read-only replicas, allowing for better performance
and availability for users."
        >
          <Collapsible
            triggerChildren={
              <div className="font-semibold text-muted">Read Replicas</div>
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
