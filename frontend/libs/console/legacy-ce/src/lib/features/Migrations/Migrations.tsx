import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { getConfirmation } from '@hasura/shared/utils';
import { Switch } from '@hasura/shared/ui';
import { useDocumentTitle } from '@hasura/shared/hooks';
import { useEffect } from 'react';
import { useNavigate } from 'react-router';
import { useMigrationStatus } from '@hasura/metadata/api';
import { useAppContext } from '@hasura/shared/context';
import { Heading } from '@radix-ui/themes';

const Migrations = () => {
  useDocumentTitle('Migrations - Data | Hasura');

  const { data: migrationStatus, updateMigrationModeStatus } =
    useMigrationStatus();
  const { envVars } = useAppContext();
  const navigate = useNavigate();

  useEffect(() => {
    if (envVars.consoleMode === 'server') {
      navigate('/data', { replace: true });
    }
  }, [envVars]);

  const handleMigrationModeToggle = () => {
    const isOk = getConfirmation();
    if (isOk) {
      updateMigrationModeStatus();
    }
  };

  const getNotesSection = () => {
    return (
      <ul>
        <li>Migrations are used to track changes to the database schema.</li>
        <li>
          If you are managing database migrations externally, it is recommended
          that you disable making schema changes via the console.
        </li>
        <li>
          Read more about managing migrations with Hasura at the{' '}
          <a
            href="https://hasura.io/docs/latest/graphql/core/migrations/index.html"
            target="_blank"
            rel="noopener noreferrer"
          >
            Hasura migrations guide
          </a>
        </li>
      </ul>
    );
  };

  return (
    <Analytics name="Migrations" {...REDACT_EVERYTHING}>
      <div>
        <Heading size="4">Database Migrations</Heading>
        <div className="mt-6">
          <div>{getNotesSection()}</div>
          <div className="mt-6">
            <Switch
              value={migrationStatus === 'healthy'}
              onChange={handleMigrationModeToggle}
            >
              Allow Postgres schema changes via console
            </Switch>
          </div>
        </div>
      </div>
    </Analytics>
  );
};

export default Migrations;
