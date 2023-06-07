import { Collapsible } from '../../../../../new-components/Collapsible';
import { isProConsole } from '../../../../../utils';
import { DatabaseUrl } from './DatabaseUrl';
import { IsolationLevel } from './IsolationLevel';
import { PoolSettings } from './PoolSettings';
import { SslSettings } from './SslSettings';
import { UsePreparedStatements } from './UsePreparedStatements';

export const ConnectionInfo = ({
  name,
  hideOptions,
}: {
  name: string;
  hideOptions: string[];
}) => {
  return (
    <div className="bg-white border border-hasGray-300 rounded-md shadow-sm overflow-hidden p-4">
      <DatabaseUrl name={`${name}.databaseUrl`} hideOptions={hideOptions} />

      <PoolSettings name={`${name}.poolSettings`} />
      <IsolationLevel name={`${name}.isolationLevel`} />
      <UsePreparedStatements name={`${name}.usePreparedStatements`} />
      {isProConsole(window.__env) && (
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
          <SslSettings name={`${name}.sslSettings`} />
        </Collapsible>
      )}
    </div>
  );
};
