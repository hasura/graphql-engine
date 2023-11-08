import React from 'react';
import { FaExclamationTriangle } from 'react-icons/fa';
import { Card } from '../../../../new-components/Card';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';
import { useEnvironmentState } from '../../hooks';
import { useConsoleConfig } from '../../../../hooks/useEnvVars';
import { DbConnectConsoleType } from '../../types';

const DOC_LINK_PER_ENV: Record<
  'server' | 'cli',
  Record<DbConnectConsoleType, string | undefined>
> = {
  cli: {
    'pro-lite': undefined,
    oss: undefined,
    pro: undefined,
    cloud: undefined,
  },
  server: {
    'pro-lite':
      'https://hasura.io/docs/latest/deployment/deployment-guides/docker/#run-the-docker-container-with-an-admin-secret-env-var',
    oss: 'https://hasura.io/docs/latest/deployment/deployment-guides/docker/#run-the-docker-container-with-an-admin-secret-env-var',
    pro: 'https://hasura.io/docs/latest/deployment/deployment-guides/docker/#run-the-docker-container-with-an-admin-secret-env-var',
    cloud: 'https://hasura.io/docs/latest/hasura-cloud/projects/env-vars/',
  },
};

export const WarningCard: React.FC<unknown> = () => {
  const { consoleType } = useEnvironmentState();
  const { mode } = useConsoleConfig();
  const docLink = DOC_LINK_PER_ENV[mode][consoleType];

  return (
    <Card mode="warning" className="mb-3">
      <div className="flex gap-4 items-center">
        <div className="text-amber-500 mr-2 rounded-full bg-amber-100 p-4">
          <FaExclamationTriangle size="28" />
        </div>
        <div>
          <div className="text-lg text-amber-500 font-semibold">Warning</div>
          <div>
            This option <strong>exposes sensitive information</strong> such as
            password and hostname <strong>in your metadata as plaintext</strong>
            .
            <br />
            The <strong>recommended way</strong> of adding connections is using
            an <strong>Environment variable</strong>.
            {docLink ? <LearnMoreLink href={docLink} /> : null}
          </div>
        </div>
      </div>
    </Card>
  );
};
export const WarningCardMetadataDBNotDynamic: React.FC<unknown> = () => {
  return (
    <Card mode="warning" className="mb-3">
      <div className="flex gap-4 items-center">
        <div className="text-amber-500 mr-2 rounded-full bg-amber-100 p-4">
          <FaExclamationTriangle size="28" />
        </div>
        <div>
          <div className="text-lg text-amber-500 font-semibold">Warning</div>
          <div>
            This will have no effect on your metadata database URI, which may
            have been initialized from{' '}
            <code className="!px-0">HASURA_GRAPHQL_DATABASE_URL</code>. If you
            need a dynamic URL for metadata as well, your administrator will
            need to set{' '}
            <code className="!px-0">
              HASURA_GRAPHQL_METADATA_DATABASE_URL=dynamic-from-file:///path/to/file
            </code>
          </div>
        </div>
      </div>
    </Card>
  );
};
