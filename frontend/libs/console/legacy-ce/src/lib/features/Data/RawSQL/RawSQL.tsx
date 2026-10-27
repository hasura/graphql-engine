import { useLayoutEffect, useRef, useState } from 'react';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import {
  AceEditor,
  Button,
  Checkbox,
  Dialog,
  FieldLabel,
  IconTooltip,
  Input,
  LearnMoreLink,
  Text,
} from '@hasura/shared/ui';
import StatementTimeout from './StatementTimeout';
import {
  checkChangeLang,
  checkTextLength,
  parseStatementTimeout,
  unsupportedRawSQLDrivers,
} from './utils';
import { CLI_CONSOLE_MODE } from '@hasura/shared/types';
import NotesSection from './molecules/NotesSection';
import ResultTable from './ResultTable';
import { getLSItem, setLSItem, extractTableInfo } from '@hasura/shared/utils';
import DropDownSelector from './DropDownSelector';
import { useRunRawSQL } from './hooks/useRunRawSQL';
import { useSearchParams } from 'react-router';
import { useDocumentTitle } from '@hasura/shared/hooks';
import { useMetadata, useMigrationStatus } from '@hasura/metadata/api';
import { isNativeDriver, MetadataSelectors } from '@hasura/metadata/helpers';
import { LS_KEYS } from '@hasura/shared/types';
import {
  getDatabaseMethods,
  removeCommentsSQL,
} from '@hasura/metadata/data-source';
import { useAppContext } from '@hasura/shared/context';
import { Em, Flex, Heading, Strong } from '@radix-ui/themes';

/**
 * # RawSQL React FC
 * ## renders raw SQL page on route `/data/sql`
 */
const RawSQL = () => {
  useDocumentTitle('Run SQL - Data | Hasura');

  const [searchParams] = useSearchParams();
  const { envVars } = useAppContext();
  const { data: meta } = useMetadata();
  const { data: migrationStatus } = useMigrationStatus();

  const [sql, setSQL] = useState(getLSItem(LS_KEYS.rawSQLKey) ?? '');
  const [isMigrationChecked, setIsMigrationChecked] = useState(false);
  const [isModalOpen, setIsModalOpen] = useState(false);
  const [isReadOnlyChecked, setIsReadOnlyChecked] = useState(false);
  const [isTableTrackChecked, setIsTableTrackChecked] = useState(false);
  const [isCascadeChecked, setIsCascadeChecked] = useState(false);
  const [migrationName, setMigrationName] = useState('run_sql_migration');

  const {
    mutate: fetchRunSQLResult,
    data,
    isPending: isLoading,
  } = useRunRawSQL();

  // a cleared timeout is stored as 0 and stays cleared after a reload
  const [statementTimeout, setStatementTimeout] = useState<number>(() => {
    const stored = getLSItem(LS_KEYS.rawSqlStatementTimeout);
    return stored ? parseStatementTimeout(stored) : 10;
  });

  const nativeSources =
    meta?.metadata?.sources?.filter((source) => isNativeDriver(source.kind)) ??
    [];

  // Metadata may still be loading on the first render, so the default source is
  // resolved on every render instead of being captured in the initial state.
  const [pickedDatabase, setPickedDatabase] = useState(
    searchParams.get('source'),
  );
  const selectedSource =
    nativeSources.find((source) => source.name === pickedDatabase) ??
    nativeSources[0];
  const selectedDatabase = selectedSource?.name;
  const selectedDriver = selectedSource?.kind;

  // `suggestLangChange` is fully derived from the SQL text and the selected
  // driver, so compute it during render instead of mirroring it into state via
  // an effect (this also removes an extra render pass).
  const suggestLangChange = checkChangeLang(sql, selectedDriver);

  const dataSource = selectedDriver ? getDatabaseMethods(selectedDriver) : null;
  const isTrackingSupported =
    dataSource?.check.isFeatureSupported('rawSQL.tracking') ?? false;
  const isCLIMode = envVars.consoleMode === CLI_CONSOLE_MODE;

  // Derived straight from `dataSource` (already computed above); a plain value
  // the React Compiler can memoize, instead of a manual useMemo it had to skip.
  const isStatementTimeoutSupported = dataSource
    ? typeof dataSource.utilities.statementTimeoutSQL === 'function'
    : null;

  // Keep the entered timeout when switching to a driver without support for
  // it, so it's still there when switching back; it's only left out of runs.
  const effectiveStatementTimeout = isStatementTimeoutSupported
    ? statementTimeout
    : null;

  const isRawSQLSupported =
    !!selectedDriver && !unsupportedRawSQLDrivers.includes(selectedDriver);

  // "Track this" is only sent while it's shown and enabled
  const isTableTracked =
    isTableTrackChecked &&
    isRawSQLSupported &&
    isTrackingSupported &&
    !suggestLangChange;

  const runSQL = (sql: string, migration?: { name: string }) => {
    if (!selectedDriver || !selectedDatabase) {
      return;
    }

    fetchRunSQLResult({
      source: {
        name: selectedDatabase,
        kind: selectedDriver,
      },
      sql,
      isTableTracked,
      cascade: isCascadeChecked,
      readOnly: isReadOnlyChecked,
      statementTimeout: effectiveStatementTimeout,
      migration,
    });
  };

  const submitSQL = (sql: string) => {
    if (!selectedDriver || !selectedDatabase) {
      return;
    }

    if (!sql) {
      setLSItem(LS_KEYS.rawSQLKey, '');
      return;
    } else if (checkTextLength(sql)) {
      // set SQL to LS
      setLSItem(LS_KEYS.rawSQLKey, sql);
    }

    let migration:
      | {
          name: string;
        }
      | undefined;

    // check migration mode global
    if (migrationStatus === 'healthy') {
      if (!isMigrationChecked && isCLIMode) {
        // if migration is not checked, check if is schema modification
        if (dataSource?.check.isSchemaModification(removeCommentsSQL(sql))) {
          setIsModalOpen(true);
          return;
        }
      }

      if (isMigrationChecked) {
        migration = { name: migrationName || 'run_sql_migration' };
      }
    }

    runSQL(sql, migration);
  };

  const suggestedChecksRef = useRef<{
    migration: boolean;
    track: boolean;
  } | null>(null);

  const submitSQLRef = useRef(submitSQL);
  useLayoutEffect(() => {
    submitSQLRef.current = submitSQL;
  });

  const getMigrationWarningModal = () => {
    const onModalClose = () => {
      setIsModalOpen(false);
    };

    const onConfirmNoMigration = () => {
      setIsModalOpen(false);
      runSQL(sql);
    };

    if (!isModalOpen) {
      return null;
    }

    return (
      <Dialog
        title="Run SQL"
        onClose={onModalClose}
        footer={{
          callToAction: 'Yes, I confirm',
          onSubmit: onConfirmNoMigration,
        }}
      >
        <div className="content-fluid">
          <div className="row">
            <div className="col-xs-12">
              Your SQL statement is most likely modifying the database schema.
              Are you sure it is not a migration?
            </div>
          </div>
        </div>
      </Dialog>
    );
  };

  const getSQLSection = () => {
    // What the SQL implies for "This is a migration" and "Track this".
    const getSuggestedChecks = (cleanSql: string) => {
      if (!dataSource?.check.isSchemaModification(cleanSql)) {
        return { migration: false, track: false };
      }

      const objects = isTrackingSupported
        ? (dataSource.utilities.parseCreateSchemaSQL?.(cleanSql) ?? [])
        : [];
      const trackedObjectNames = (
        meta && selectedDatabase
          ? MetadataSelectors.getTables(selectedDatabase)(meta)
          : []
      )
        .map((t) => extractTableInfo(t.table))
        .filter((info): info is NonNullable<typeof info> => Boolean(info))
        .map((info) => [info.schema, info.name].join('.'));

      // functions are left out: they may only be meant as computed fields
      const allObjectsTrackable = objects.every(
        (object) =>
          object.type !== 'function' &&
          !trackedObjectNames.includes([object.schema, object.name].join('.')),
      );

      return {
        migration: true,
        track: objects.length > 0 && allObjectsTrackable,
      };
    };

    const handleSQLChange = (val: string) => {
      setSQL(val);

      if (!selectedDriver) {
        return;
      }

      // Only apply a suggestion when it changes, so a checkbox the user
      // toggled isn't reset on every keystroke.
      const suggested = getSuggestedChecks(removeCommentsSQL(val));
      const previous = suggestedChecksRef.current;
      if (suggested.migration !== previous?.migration) {
        setIsMigrationChecked(suggested.migration);
      }
      if (suggested.track !== previous?.track) {
        setIsTableTrackChecked(suggested.track);
      }
      suggestedChecksRef.current = suggested;
    };

    return (
      <div className="mt-6">
        <AceEditor
          data-test="sql-test-editor"
          mode="sql"
          name="raw_sql"
          value={sql}
          minLines={15}
          maxLines={100}
          width="100%"
          showPrintMargin={false}
          commands={[
            {
              name: 'submit',
              bindKey: { win: 'Ctrl-Enter', mac: 'Command-Enter' },
              // react-ace registers commands only on mount, so this goes
              // through a ref to reach the latest state. The editor text is
              // read directly since `onChange` is debounced.
              exec: ((editor) => {
                const value = editor?.getValue();
                if (value) {
                  setSQL(value);
                  submitSQLRef.current(value);
                }
              }) as any,
            },
          ]}
          onChange={handleSQLChange}
          // prevents unwanted frequent event triggers
          debounceChangePeriod={200}
          setOptions={{ useWorker: false }}
        />
      </div>
    );
  };

  const getMetadataCascadeSection = () => {
    return (
      <Flex align="center" gap="2" className="mt-3">
        <Checkbox
          value={isCascadeChecked}
          id="cascade-checkbox"
          onChange={(checked) => {
            setIsCascadeChecked(checked === true);
          }}
        >
          Cascade metadata
        </Checkbox>
        <IconTooltip
          message={
            'Cascade actions on all dependent metadata references, like relationships and permissions'
          }
        />
      </Flex>
    );
  };

  const getReadOnlySection = () => {
    return (
      <Flex className="mt-3" gap="2">
        <Checkbox
          value={isReadOnlyChecked}
          id="read-only-checkbox"
          onChange={(checked) => {
            setIsReadOnlyChecked(checked === true);
          }}
        >
          Read only
        </Checkbox>
        <IconTooltip
          message={
            'When set to true, the request will be run in READ ONLY transaction access mode which means only select queries will be successful. This flag ensures that the GraphQL schema is not modified and is hence highly performant.'
          }
        />
      </Flex>
    );
  };

  const getTrackThisSection = () => {
    const dispatchTrackThis = () => {
      setIsTableTrackChecked((prev) => !prev);
    };

    const isDisabled = suggestLangChange;

    return (
      <Flex className="mt-6" align="center" gap="2">
        <Checkbox
          value={isTableTrackChecked}
          id="track-checkbox"
          disabled={isDisabled}
          onChange={dispatchTrackThis}
          data-test="raw-sql-track-check"
        >
          Track this
        </Checkbox>
        <IconTooltip
          message={
            'If you are creating tables, views or functions, checking this will also expose them over the GraphQL API as ' +
            'top level fields. Functions only intended to be used as computed fields should not be tracked.'
          }
        />

        <LearnMoreLink
          text={'(See supported functions requirements)'}
          href={
            'https://hasura.io/docs/latest/graphql/core/schema/custom-functions.html#supported-sql-functions'
          }
        />
      </Flex>
    );
  };

  const getMigrationSection = () => {
    const getIsMigrationSection = () => {
      return (
        <Flex align="center" gap="2">
          <Checkbox
            value={isMigrationChecked}
            id="migration-checkbox"
            onChange={(checked) => setIsMigrationChecked(checked === true)}
            data-test="raw-sql-migration-check"
          >
            This is a migration
          </Checkbox>
          <IconTooltip
            message={'Create a migration file with the SQL statement'}
          />
        </Flex>
      );
    };

    const getMigrationNameSection = () => {
      if (!isMigrationChecked) {
        return null;
      }

      return (
        <div className={'mt-3'}>
          <div>
            <FieldLabel label="Migration name:" />
            <Input
              placeholder="run_sql_migration"
              id="migration-name"
              type="text"
              onChange={(e) => setMigrationName(e.target.value)}
            />
            <IconTooltip
              message={
                "Name of the generated migration file. Default: 'run_sql_migration'"
              }
            />
            <div className="mt-3">
              <Text as="p">
                <Em>
                  Note: down migration will not be generated for statements run
                  using Raw SQL.
                </Em>
              </Text>
            </div>
          </div>
        </div>
      );
    };

    if (migrationStatus === 'healthy') {
      return (
        <div className="mt-3">
          {getIsMigrationSection()}
          {getMigrationNameSection()}
        </div>
      );
    }

    return null;
  };

  const updateStatementTimeout = (value: string) => {
    const timeoutInSeconds = parseStatementTimeout(value);
    setLSItem(LS_KEYS.rawSqlStatementTimeout, timeoutInSeconds.toString());
    setStatementTimeout(timeoutInSeconds);
  };

  return (
    <Analytics name="RawSQL" {...REDACT_EVERYTHING}>
      <div className="p-6">
        <Heading size="4">Raw SQL</Heading>
        <div className="my-4 pl-4">
          <NotesSection suggestLangChange={suggestLangChange} />
        </div>
        <div>
          <div>
            <Text as="p">
              <Strong>Database</Strong>
            </Text>{' '}
            <div className="mt-2 sm:w-1/2">
              <DropDownSelector
                options={nativeSources.map((source) => ({
                  name: source.name,
                  driver: source.kind,
                }))}
                value={selectedDatabase}
                onChange={setPickedDatabase}
              />
            </div>
          </div>
          <div className="my-4">{getSQLSection()}</div>
          <div>
            {!isRawSQLSupported ? null : (
              <>
                {isTrackingSupported ? getTrackThisSection() : null}
                {getMetadataCascadeSection()}
                {getReadOnlySection()}
                {getMigrationSection()}
              </>
            )}

            {isStatementTimeoutSupported && (
              <StatementTimeout
                statementTimeout={statementTimeout}
                isCliMode={isCLIMode}
                isMigrationChecked={isCLIMode && isMigrationChecked}
                updateStatementTimeout={updateStatementTimeout}
              />
            )}
            <div className="mt-6">
              <Button
                type="submit"
                onClick={() => submitSQL(sql)}
                mode="primary"
                data-test="run-sql"
                disabled={!sql?.length || !isRawSQLSupported}
                loading={isLoading}
                loadingText="Running..."
              >
                Run!
              </Button>
            </div>
          </div>
        </div>
        {getMigrationWarningModal()}
        <div>
          {data?.length ? (
            <ResultTable rows={data.slice(1)} headers={data[0]} />
          ) : null}
        </div>
      </div>
    </Analytics>
  );
};

export default RawSQL;
