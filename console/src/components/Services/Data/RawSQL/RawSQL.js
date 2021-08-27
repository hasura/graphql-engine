import React, { useEffect, useState } from 'react';
import PropTypes from 'prop-types';
import Helmet from 'react-helmet';
import AceEditor from 'react-ace';
import 'brace/mode/sql';

import Modal from '../../../Common/Modal/Modal';
import Button from '../../../Common/Button/Button';
import Tooltip from '../../../Common/Tooltip/Tooltip';
import KnowMoreLink from '../../../Common/KnowMoreLink/KnowMoreLink';
import Alert from '../../../Common/Alert';
import StatementTimeout from './StatementTimeout';
import { parseCreateSQL, removeCommentsSQL } from './utils';
import styles from '../../../Common/TableCommon/Table.scss';
import {
  executeSQL,
  SET_SQL,
  SET_CASCADE_CHECKED,
  SET_MIGRATION_CHECKED,
  SET_TRACK_TABLE_CHECKED,
} from './Actions';
import { modalOpen, modalClose } from './Actions';
import globals from '../../../../Globals';
import {
  ACE_EDITOR_THEME,
  ACE_EDITOR_FONT_SIZE,
} from '../../../Common/AceEditor/utils';
import { CLI_CONSOLE_MODE } from '../../../../constants';
import NotesSection from './molecules/NotesSection';
import ResultTable from './ResultTable';
import { getLSItem, setLSItem, LS_KEYS } from '../../../../utils/localStorage';
import DropDownSelector from './DropDownSelector';
import { getSourceDriver } from '../utils';
import { getDataSources } from '../../../../metadata/selector';
import { services } from '../../../../dataSources/services';
import { isFeatureSupported, setDriver } from '../../../../dataSources';
import { fetchDataInit, UPDATE_CURRENT_DATA_SOURCE } from '../DataActions';

const checkChangeLang = (sql, selectedDriver) => {
  return (
    !sql?.match(/(?:\$\$\s+)?language\s+plpgsql/i) && selectedDriver === 'citus'
  );
};

/**
 * # RawSQL React FC
 * ## renders raw SQL page on route `/data/sql`
 *
 * @typedef Props
 * @property {string} sql
 * @property {string} resultType
 * @property {array} result
 * @property {array} resultHeaders
 * @property {function} dispatch
 * @property {boolean} ongoingRequest
 * @property {object} lastError
 * @property {boolean} lastSuccess
 * @property {boolean} isModalOpen
 * @property {boolean} isCascadeChecked
 * @property {boolean} isMigrationChecked
 * @property {boolean} isTableTrackChecked
 * @property {boolean} migrationMode
 * @property {array} allSchemas
 *
 * @param {Props}
 */
const RawSQL = ({
  sql,
  resultType,
  result,
  resultHeaders,
  dispatch,
  ongoingRequest,
  lastError,
  lastSuccess,
  isModalOpen,
  isCascadeChecked,
  isMigrationChecked,
  isTableTrackChecked,
  migrationMode,
  allSchemas,
  sources,
  currentDataSource,
}) => {
  const [statementTimeout, setStatementTimeout] = useState(
    Number(getLSItem(LS_KEYS.rawSqlStatementTimeout)) || 10
  );

  const [sqlText, onChangeSQLText] = useState(sql);

  const [selectedDatabase, setSelectedDatabase] = useState(currentDataSource);
  const [selectedDriver, setSelectedDriver] = useState('postgres');
  const [suggestLangChange, setSuggestLangChange] = useState(false);

  useEffect(() => {
    const driver = getSourceDriver(sources, selectedDatabase);
    setSelectedDriver(driver);
    if (!isFeatureSupported('rawSQL.statementTimeout'))
      setStatementTimeout(null);
  }, [selectedDatabase, sources]);

  const dropDownSelectorValueChange = value => {
    const driver = getSourceDriver(sources, value);
    dispatch({
      type: UPDATE_CURRENT_DATA_SOURCE,
      source: value,
    });
    setDriver(driver);
    dispatch(fetchDataInit(value, driver));

    setSelectedDatabase(value);
  };

  useEffect(() => {
    if (!sql) {
      const sqlFromLocalStorage = getLSItem(LS_KEYS.rawSQLKey);
      if (sqlFromLocalStorage) {
        dispatch({ type: SET_SQL, data: sqlFromLocalStorage });
        onChangeSQLText(sqlFromLocalStorage);
      }
    }
    return () => {
      setLSItem(LS_KEYS.rawSQLKey, sqlText);
    };
  }, [dispatch, sql, sqlText]);

  useEffect(() => {
    if (checkChangeLang(sql, selectedDriver)) {
      setSuggestLangChange(true);
    } else {
      setSuggestLangChange(false);
    }
  }, [sql, selectedDriver]);

  const submitSQL = () => {
    if (!sqlText) {
      setLSItem(LS_KEYS.rawSQLKey, '');
      return;
    }
    // set SQL to LS
    setLSItem(LS_KEYS.rawSQLKey, sqlText);

    // check migration mode global
    if (migrationMode) {
      const checkboxElem = document.getElementById('migration-checkbox');
      const isMigration = checkboxElem ? checkboxElem.checked : false;
      const textboxElem = document.getElementById('migration-name');
      let migrationName = textboxElem ? textboxElem.value : '';
      if (isMigration && migrationName.length === 0) {
        migrationName = 'run_sql_migration';
      }
      if (!isMigration && globals.consoleMode === CLI_CONSOLE_MODE) {
        // if migration is not checked, check if is schema modification
        if (services[selectedDriver].checkSchemaModification(sqlText)) {
          dispatch(modalOpen());
          return;
        }
      }
      dispatch(
        executeSQL(
          isMigration,
          migrationName,
          statementTimeout,
          selectedDatabase,
          selectedDriver
        )
      );
      return;
    }
    dispatch(executeSQL(false, '', statementTimeout, selectedDatabase));
  };

  const getMigrationWarningModal = () => {
    const onModalClose = () => {
      dispatch(modalClose());
    };

    const onConfirmNoMigration = () => {
      const isMigration = document.getElementById('migration-checkbox').checked;
      dispatch(modalClose());
      dispatch(executeSQL(isMigration));
    };

    return (
      <Modal
        show={isModalOpen}
        title={'Run SQL'}
        onClose={onModalClose}
        onSubmit={onConfirmNoMigration}
        submitText={'Yes, I confirm'}
        submitTestId={'not-migration-confirm'}
      >
        <div className="content-fluid">
          <div className="row">
            <div className="col-xs-12">
              Your SQL statement is most likely modifying the database schema.
              Are you sure it is not a migration?
            </div>
          </div>
        </div>
      </Modal>
    );
  };

  const getSQLSection = () => {
    const handleSQLChange = val => {
      const cleanSql = removeCommentsSQL(val);
      onChangeSQLText(val);
      dispatch({ type: SET_SQL, data: val });

      // set migration checkbox true
      if (services[selectedDriver].checkSchemaModification(cleanSql)) {
        dispatch({ type: SET_MIGRATION_CHECKED, data: true });
      } else {
        dispatch({ type: SET_MIGRATION_CHECKED, data: false });
      }

      // set track this checkbox true
      const objects = parseCreateSQL(cleanSql, selectedDriver);
      if (objects.length) {
        let allObjectsTrackable = true;

        const trackedObjectNames = allSchemas.map(schema => {
          return [schema.table_schema, schema.table_name].join('.');
        });

        allObjectsTrackable = objects.every(object => {
          if (object.type === 'function') {
            return false;
          }

          const objectName = [object.schema, object.name].join('.');

          if (trackedObjectNames.includes(objectName)) {
            return false;
          }

          return true;
        });

        if (allObjectsTrackable) {
          dispatch({ type: SET_TRACK_TABLE_CHECKED, data: true });
        } else {
          dispatch({ type: SET_TRACK_TABLE_CHECKED, data: false });
        }
      } else {
        dispatch({ type: SET_TRACK_TABLE_CHECKED, data: false });
      }
    };

    return (
      <div className={styles.add_mar_top}>
        <AceEditor
          data-test="sql-test-editor"
          mode="sql"
          theme={ACE_EDITOR_THEME}
          fontSize={ACE_EDITOR_FONT_SIZE}
          name="raw_sql"
          value={sqlText}
          minLines={15}
          maxLines={100}
          width="100%"
          showPrintMargin={false}
          commands={[
            {
              name: 'submit',
              bindKey: { win: 'Ctrl-Enter', mac: 'Command-Enter' },
              exec: () => {
                if (sqlText) {
                  submitSQL();
                }
              },
            },
          ]}
          onChange={handleSQLChange}
          // prevents unwanted frequent event triggers
          debounceChangePeriod={200}
        />
      </div>
    );
  };

  const getMetadataCascadeSection = () => {
    return (
      <div className={styles.add_mar_top_small}>
        <label>
          <input
            checked={isCascadeChecked}
            className={`${styles.add_mar_right_small} ${styles.cursorPointer} legacy-input-fix`}
            id="cascade-checkbox"
            type="checkbox"
            onChange={() => {
              dispatch({
                type: SET_CASCADE_CHECKED,
                data: !isCascadeChecked,
              });
            }}
          />
          Cascade metadata
        </label>
        <Tooltip
          message={
            'Cascade actions on all dependent metadata references, like relationships and permissions'
          }
        />
      </div>
    );
  };

  const getTrackThisSection = () => {
    const dispatchTrackThis = () => {
      dispatch({
        type: SET_TRACK_TABLE_CHECKED,
        data: !isTableTrackChecked,
      });
    };

    return (
      <div className={styles.add_mar_top}>
        {isFeatureSupported('rawSQL.tracking') && (
          <label>
            <input
              checked={isTableTrackChecked}
              className={`${styles.add_mar_right_small} ${styles.cursorPointer} legacy-input-fix`}
              id="track-checkbox"
              type="checkbox"
              disabled={checkChangeLang()}
              onChange={dispatchTrackThis}
              data-test="raw-sql-track-check"
            />
            Track this
          </label>
        )}
        <Tooltip
          message={
            'If you are creating tables, views or functions, checking this will also expose them over the GraphQL API as top level fields'
          }
        />
        &nbsp;
        <KnowMoreLink
          text={'See supported functions requirements'}
          href={
            'https://hasura.io/docs/latest/graphql/core/schema/custom-functions.html#supported-sql-functions'
          }
        />
      </div>
    );
  };

  const getMigrationSection = () => {
    let migrationSection = null;

    const getIsMigrationSection = () => {
      const dispatchIsMigration = () => {
        dispatch({
          type: SET_MIGRATION_CHECKED,
          data: !isMigrationChecked,
        });
      };

      return (
        <div>
          <label>
            <input
              checked={isMigrationChecked}
              className={`${styles.add_mar_right_small} legacy-input-fix`}
              id="migration-checkbox"
              type="checkbox"
              onChange={dispatchIsMigration}
              data-test="raw-sql-migration-check"
            />
            This is a migration
          </label>
          <Tooltip message={'Create a migration file with the SQL statement'} />
        </div>
      );
    };

    const getMigrationNameSection = () => {
      let migrationNameSection = null;

      if (isMigrationChecked) {
        migrationNameSection = (
          <div className={styles.add_mar_top_small + ' ' + styles.add_mar_left}>
            <div>
              <label className={styles.add_mar_right}>Migration name:</label>
              <input
                className={`${styles.inline_block} ${styles.tableNameInput} ${styles.add_mar_right_small} form-control`}
                placeholder={'run_sql_migration'}
                id="migration-name"
                type="text"
              />
              <Tooltip
                message={
                  "Name of the generated migration file. Default: 'run_sql_migration'"
                }
              />
              <div
                className={styles.add_mar_top_small + ' ' + styles.text_gray}
              >
                <i>
                  Note: down migration will not be generated for statements run
                  using Raw SQL.
                </i>
              </div>
            </div>
          </div>
        );
      }

      return migrationNameSection;
    };

    if (migrationMode && globals.consoleMode === CLI_CONSOLE_MODE) {
      migrationSection = (
        <div className={styles.add_mar_top_small}>
          {getIsMigrationSection()}
          {getMigrationNameSection()}
        </div>
      );
    }

    return migrationSection;
  };

  const updateStatementTimeout = value => {
    const timeoutInSeconds = Number(value.trim());
    const isValidTimeout = timeoutInSeconds > 0 && !isNaN(timeoutInSeconds);
    setLSItem(LS_KEYS.rawSqlStatementTimeout, timeoutInSeconds);
    setStatementTimeout(isValidTimeout ? timeoutInSeconds : 0);
  };

  return (
    <div
      className={`${styles.clear_fix} ${styles.padd_left} ${styles.padd_top}`}
    >
      <Helmet title="Run SQL - Data | Hasura" />
      <div className={styles.subHeader}>
        <h2 className={`${styles.heading_text} ${styles.remove_pad_bottom}`}>
          Raw SQL
        </h2>
        <div className="clearfix" />
      </div>
      <div className={styles.add_mar_top}>
        <div className={`${styles.padd_left_remove} col-xs-8`}>
          <NotesSection suggestLangChange={suggestLangChange} />
        </div>
        <div className={`${styles.padd_left_remove} col-xs-8`}>
          <label>
            <b>Database</b>
          </label>{' '}
          <DropDownSelector
            options={sources.map(source => source.name)}
            defaultValue={currentDataSource}
            onChange={dropDownSelectorValueChange}
          />
        </div>
        <div className={`${styles.padd_left_remove} col-xs-10`}>
          {getSQLSection()}
        </div>

        <div
          className={`${styles.padd_left_remove} ${styles.add_mar_bottom} col-xs-8`}
        >
          {getTrackThisSection()}
          {getMetadataCascadeSection()}
          {getMigrationSection()}

          {isFeatureSupported('rawSQL.statementTimeout') && (
            <StatementTimeout
              statementTimeout={statementTimeout}
              isMigrationChecked={
                globals.consoleMode === CLI_CONSOLE_MODE && isMigrationChecked
              }
              updateStatementTimeout={updateStatementTimeout}
            />
          )}
          <Button
            type="submit"
            className={styles.add_mar_top}
            onClick={submitSQL}
            color="yellow"
            size="sm"
            data-test="run-sql"
            disabled={!sqlText.length}
          >
            Run!
          </Button>
        </div>

        <div className="hidden col-xs-4">
          <div className={`${styles.padd_left_remove} col-xs-12`}>
            {ongoingRequest && <Alert type="warning" text="Running..." />}
            {lastError && (
              <Alert
                type="danger"
                text={`Error: ${JSON.stringify(lastError)}`}
              />
            )}
            {lastSuccess && <Alert type="success" text="Executed Query" />};
          </div>
        </div>
      </div>

      {getMigrationWarningModal()}

      <div className={styles.add_mar_bottom}>
        {resultType &&
          resultType !== 'command' &&
          result &&
          result?.length > 0 && (
            <ResultTable rows={result} headers={resultHeaders} />
          )}
      </div>
    </div>
  );
};

RawSQL.propTypes = {
  sql: PropTypes.string.isRequired,
  resultType: PropTypes.string.isRequired,
  result: PropTypes.array.isRequired,
  resultHeaders: PropTypes.array.isRequired,
  allSchemas: PropTypes.array.isRequired,
  dispatch: PropTypes.func.isRequired,
  ongoingRequest: PropTypes.bool.isRequired,
  lastError: PropTypes.object.isRequired,
  lastSuccess: PropTypes.bool.isRequired,
  isModalOpen: PropTypes.bool.isRequired,
  isMigrationChecked: PropTypes.bool.isRequired,
  isTableTrackChecked: PropTypes.bool.isRequired,
  migrationMode: PropTypes.bool.isRequired,
  currentSchema: PropTypes.string.isRequired,
  statementTimeout: PropTypes.string.isRequired,
};

const mapStateToProps = state => ({
  ...state.rawSQL,
  migrationMode: state.main.migrationMode,
  currentSchema: state.tables.currentSchema,
  allSchemas: state.tables.allSchemas,
  serverVersion: state.main.serverVersion ? state.main.serverVersion : '',
  sources: getDataSources(state),
  currentDataSource: state.tables.currentDataSource,
});

const rawSQLConnector = connect => connect(mapStateToProps)(RawSQL);

export default rawSQLConnector;
