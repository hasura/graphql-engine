import { Card, Collapsible, Link, Text } from '@hasura/shared/ui';
import { isProConsole } from '@hasura/shared/utils';
import { DatabaseUrl } from './DatabaseUrl';
import { IsolationLevel } from './IsolationLevel';
import { PoolSettings } from './PoolSettings';
import { SslSettings } from './SslSettings';
import { UsePreparedStatements } from './UsePreparedStatements';
import { Strong } from '@radix-ui/themes';

export const ConnectionInfo = ({
  name,
  hideOptions,
}: {
  name: string;
  hideOptions: string[];
}) => {
  return (
    <Card>
      <DatabaseUrl name={`${name}.databaseUrl`} hideOptions={hideOptions} />

      <PoolSettings name={`${name}.poolSettings`} />
      <IsolationLevel name={`${name}.isolationLevel`} />
      <UsePreparedStatements name={`${name}.usePreparedStatements`} />
      {isProConsole(window.__env) && (
        <Collapsible
          triggerChildren={
            <Text>
              <Strong>SSL Certificates Settings</Strong>
              <Text className="italic">
                (Certificates will be loaded from{' '}
                <Link
                  href="https://hasura.io/docs/2.0/databases/postgres/gcp/#step-72-add-env-vars"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  environment variables
                </Link>
                )
              </Text>
            </Text>
          }
        >
          <SslSettings name={`${name}.sslSettings`} />
        </Collapsible>
      )}
    </Card>
  );
};
