import { useAppContext } from '@hasura/shared/context';
import { HttpError } from '@hasura/shared/types';
import { IndicatorCard, Text } from '@hasura/shared/ui';

/**
 * Whether a failed schema change may still have been applied. A 4xx means the
 * request was rejected before anything ran (for the CLI, `data_api_error` is
 * the database refusing the SQL, which runs in a transaction). A 5xx or a lost
 * response can come after the SQL ran, e.g. when the CLI could not record the
 * migration version.
 */
export const mayHaveBeenApplied = (error: unknown) =>
  error instanceof HttpError &&
  (error.status === 'network-error' || error.status >= 500);

type MigrationRequestNoticeProps = {
  isPending: boolean;
  error: unknown;
};

/**
 * Explains what a column change is doing while it runs and, when it fails in a
 * way that may have left the database changed, what to check before retrying.
 */
export const MigrationRequestNotice = ({
  isPending,
  error,
}: MigrationRequestNoticeProps) => {
  const { envVars } = useAppContext();
  const isCli = envVars.consoleMode === 'cli';

  if (isPending && isCli) {
    return (
      <Text as="p" size="2" color="gray" className="mt-2">
        Applying the change and writing its migration through the Hasura CLI.
        Keep this dialog open until it finishes.
      </Text>
    );
  }

  if (!isPending && mayHaveBeenApplied(error)) {
    return (
      <IndicatorCard
        status="negative"
        headline="The change may already be in the database"
        className="mt-2"
      >
        <Text as="p" size="2">
          The request failed after it was sent, so the SQL may have run anyway.
          The column list has been refreshed: check it before retrying.
        </Text>
        {isCli && (
          <Text as="p" size="2" className="mt-1">
            The migration file may not have been written. Run{' '}
            <code>hasura migrate status</code> and look in your migrations
            folder before retrying, and see the CLI&apos;s output for the cause.
          </Text>
        )}
      </IndicatorCard>
    );
  }

  return null;
};
