import { useEffect, useState } from 'react';
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
  getSourceDriver,
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

  const [statementTimeout, setStatementTimeout] = useState<number | null>(
    Number(getLSItem(LS_KEYS.rawSqlStatementTimeout)) || 10,
  );

  const nativeSources =
    meta?.metadata?.sources?.filter((source) => isNativeDriver(source.kind)) ??
    [];

  const [selectedDatabase, setSelectedDatabase] = useState(
    searchParams.get('source') || nativeSources?.[0]?.name,
  );
  const [suggestLangChange, setSuggestLangChange] = useState(false);
  const selectedDriver = nativeSources.find(
    (source) => source.name === selectedDatabase,
  )?.kind;

  const dataSource = selectedDriver ? getDatabaseMethods(selectedDriver) : null;
  const isTrackingSupported =
    dataSource?.check.isFeatureSupported('rawSQL.tracking') ?? false;
  const isCLIMode = envVars.consoleMode === CLI_CONSOLE_MODE;

  // Derived straight from `dataSource` (already computed above); a plain value
  // the React Compiler can memoize, instead of a manual useMemo it had to skip.
  const isStatementTimeoutSupported = dataSource
    ? typeof dataSource.utilities.statementTimeoutSQL === 'function'
    : null;

  useEffect(() => {
    if (isStatementTimeoutSupported === false) {
      setStatementTimeout(null);
    }
  }, [isStatementTimeoutSupported]);

  const dropDownSelectorValueChange = (value: string) => {
    const driver = getSourceDriver(nativeSources, value);
    setSelectedDatabase(driver);
  };

  useEffect(() => {
    if (checkChangeLang(sql, selectedDriver)) {
      setSuggestLangChange(true);
    } else if (suggestLangChange) {
      setSuggestLangChange(false);
    }
  }, [sql, selectedDriver]);

  const submitSQL = () => {
    if (!selectedDriver) {
      return;
    }

    if (!isNativeDriver(selectedDriver)) {
      fetchRunSQLResult({
        source: {
          name: selectedDatabase,
          kind: selectedDriver,
        },
        sql,
        isTableTracked: isTableTrackChecked,
        cascade: isCascadeChecked,
        readOnly: isReadOnlyChecked,
        statementTimeout,
      });
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
        if (dataSource?.check.isSchemaModification(sql)) {
          setIsModalOpen(true);
          return;
        }
      }

      migration = {
        name: migrationName || 'run_sql_migration',
      };
    }

    fetchRunSQLResult({
      source: {
        name: selectedDatabase,
        kind: selectedDriver,
      },
      sql,
      isTableTracked: isTableTrackChecked,
      cascade: isCascadeChecked,
      readOnly: isReadOnlyChecked,
      statementTimeout,
      migration,
    });
    // navigate('/data/sql');
  };

  const getMigrationWarningModal = () => {
    const onModalClose = () => {
      setIsModalOpen(false);
    };

    const onConfirmNoMigration = () => {
      setIsModalOpen(false);
      if (!selectedDriver) {
        return;
      }

      fetchRunSQLResult({
        source: {
          name: selectedDatabase,
          kind: selectedDriver,
        },
        sql,
        isTableTracked: isTableTrackChecked,
        cascade: isCascadeChecked,
        readOnly: isReadOnlyChecked,
        statementTimeout,
      });
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
    const handleSQLChange = (val) => {
      const cleanSql = removeCommentsSQL(val);
      setSQL(val);

      if (!selectedDriver) {
        return;
      }

      // set migration checkbox true
      if (!dataSource?.check.isSchemaModification(cleanSql)) {
        setIsMigrationChecked(false);
        return;
      }

      setIsMigrationChecked(true);
      // set track this checkbox true if tracking is supported for the driver
      if (isTrackingSupported) {
        const objects = dataSource?.utilities.parseCreateSchemaSQL
          ? dataSource?.utilities.parseCreateSchemaSQL(cleanSql)
          : [];
        if (objects?.length) {
          let allObjectsTrackable = true;

          const trackedTables = (
            meta ? MetadataSelectors.getTables(selectedDatabase)(meta) : []
          )
            .map((t) => extractTableInfo(t.table)!)
            .filter(Boolean);
          const trackedObjectNames = trackedTables.map((schema) => {
            return [schema.schema, schema.name].join('.');
          });

          allObjectsTrackable = objects.every((object) => {
            if (object.type === 'function') {
              return false;
            }

            const objectName = [object.schema, object.name].join('.');

            if (trackedObjectNames.includes(objectName)) {
              return false;
            }

            return true;
          });

          setIsTableTrackChecked(allObjectsTrackable);
        } else {
          setIsTableTrackChecked(false);
        }
      }
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
              exec: ((editor) => {
                if (editor?.getValue()) {
                  submitSQL();
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

  const updateStatementTimeout = (value) => {
    const timeoutInSeconds = Number(value.trim());
    const isValidTimeout = timeoutInSeconds > 0 && !isNaN(timeoutInSeconds);

    setLSItem(LS_KEYS.rawSqlStatementTimeout, timeoutInSeconds.toString());
    setStatementTimeout(isValidTimeout ? timeoutInSeconds : 0);
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
                defaultValue={selectedDatabase}
                onChange={dropDownSelectorValueChange}
              />
            </div>
          </div>
          <div className="my-4">{getSQLSection()}</div>
          <div>
            {!selectedDriver ||
            unsupportedRawSQLDrivers.includes(selectedDriver) ? null : (
              <>
                {isTrackingSupported ? getTrackThisSection() : null}
                {getMetadataCascadeSection()}
                {getReadOnlySection()}
                {getMigrationSection()}
              </>
            )}

            {statementTimeout !== null && isStatementTimeoutSupported && (
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
                onClick={submitSQL}
                mode="default"
                data-test="run-sql"
                disabled={
                  !sql?.length ||
                  !selectedDriver ||
                  unsupportedRawSQLDrivers.includes(selectedDriver)
                }
                loading={isLoading}
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
