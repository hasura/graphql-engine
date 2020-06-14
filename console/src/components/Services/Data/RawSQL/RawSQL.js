import React, { useEffect, useRef } from 'react';
import PropTypes from 'prop-types';
import Helmet from 'react-helmet';
import AceEditor from 'react-ace';
import 'brace/mode/sql';
import Modal from '../../../Common/Modal/Modal';
import Button from '../../../Common/Button/Button';
import { parseCreateSQL } from './utils';
import { checkSchemaModification } from '../../../Common/utils/sqlUtils';

import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import {
  executeSQL,
  SET_SQL,
  SET_CASCADE_CHECKED,
  SET_MIGRATION_CHECKED,
  SET_TRACK_TABLE_CHECKED,
  setRawSqlTimeout,
} from './Actions';
import { modalOpen, modalClose } from './Actions';
import globals from '../../../../Globals';
import {
  ACE_EDITOR_THEME,
  ACE_EDITOR_FONT_SIZE,
} from '../../../Common/AceEditor/utils';
import { CLI_CONSOLE_MODE } from '../../../../constants';
import NotesSection from './molecules/NotesSection';
import Alert from '../../../Common/Alert';

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
}) => {
  const styles = require('../../../Common/TableCommon/Table.scss');

  // local storage key for SQL
  const LS_RAW_SQL_SQL = 'rawSql:sql';

  /* hooks */

  // set up sqlRef to use in unmount
  const sqlRef = useRef(sql);

  useEffect(() => {
    if (!sql) {
      const sqlFromLocalStorage = localStorage.getItem(LS_RAW_SQL_SQL);
      if (sqlFromLocalStorage) {
        dispatch({ type: SET_SQL, data: sqlFromLocalStorage });
      }
    }
    return () => {
      localStorage.setItem(LS_RAW_SQL_SQL, sqlRef.current);
    };
  }, []);
  // set SQL to sqlRef
  useEffect(() => {
    sqlRef.current = sql;
  }, [sql]);

  /* hooks - end */

  const cascadeTip = (
    <Tooltip id="tooltip-cascade">
      Cascade actions on all dependent metadata references, like relationships
      and permissions
    </Tooltip>
  );
  const migrationTip = (
    <Tooltip id="tooltip-migration">
      Create a migration file with the SQL statement
    </Tooltip>
  );
  const migrationNameTip = (
    <Tooltip id="tooltip-migration">
      Name of the generated migration file. Default: 'run_sql_migration'
    </Tooltip>
  );
  const trackTableTip = () => (
    <Tooltip id="tooltip-tracktable">
      If you are creating a table/view/function, checking this will also expose
      them over the GraphQL API
    </Tooltip>
  );

  const statementTimeoutTip = (
    <Tooltip id="tooltip-statement-timeout">
      Abort queries that take longer than the specified time
    </Tooltip>
  );

  const submitSQL = () => {
    // set SQL to LS
    localStorage.setItem(LS_RAW_SQL_SQL, sql);

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
        if (checkSchemaModification(sql)) {
          dispatch(modalOpen());
          const confirmation = false;
          if (confirmation) {
            dispatch(executeSQL(isMigration, migrationName));
          }
        } else {
          dispatch(executeSQL(isMigration, migrationName));
        }
      } else {
        dispatch(executeSQL(isMigration, migrationName));
      }
    } else {
      dispatch(executeSQL(false, ''));
    }
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
      dispatch({ type: SET_SQL, data: val });

      // set migration checkbox true
      if (checkSchemaModification(val)) {
        dispatch({ type: SET_MIGRATION_CHECKED, data: true });
      } else {
        dispatch({ type: SET_MIGRATION_CHECKED, data: false });
      }

      // set track this checkbox true
      const objects = parseCreateSQL(val);
      if (objects.length) {
        let allObjectsTrackable = true;

        const trackedObjectNames = allSchemas.map(schema => {
          return [schema.table_schema, schema.table_name].join('.');
        });

        for (let i = 0; i < objects.length; i++) {
          const object = objects[i];

          if (object.type === 'function') {
            allObjectsTrackable = false;
            break;
          } else {
            const objectName = [object.schema, object.name].join('.');

            if (trackedObjectNames.includes(objectName)) {
              allObjectsTrackable = false;
              break;
            }
          }
        }

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
          value={sql}
          minLines={15}
          maxLines={100}
          width="100%"
          showPrintMargin={false}
          commands={[
            {
              name: 'submit',
              bindKey: { win: 'Ctrl-Enter', mac: 'Command-Enter' },
              exec: () => {
                submitSQL();
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

  const getResultTable = () => {
    let resultTable = null;

    if (resultType && resultType !== 'command') {
      const getTableHeadings = () => {
        return resultHeaders.map((columnName, i) => (
          <th key={i}>{columnName}</th>
        ));
      };

      const getRows = () => {
        return result.map((row, i) => (
          <tr key={i}>
            {row.map((columnValue, j) => (
              <td key={j}>{columnValue}</td>
            ))}
          </tr>
        ));
      };

      resultTable = (
        <div
          className={`${styles.addCol} col-xs-12 ${styles.padd_left_remove}`}
        >
          <h4 className={styles.subheading_text}>SQL Result:</h4>
          <div className={styles.tableContainer}>
            <table
              className={`table table-bordered table-striped table-hover ${styles.table} `}
            >
              <thead>
                <tr>{getTableHeadings()}</tr>
              </thead>
              <tbody>{getRows()}</tbody>
            </table>
          </div>
          <br />
          <br />
        </div>
      );
    }

    return resultTable;
  };

  const getMetadataCascadeSection = () => {
    return (
      <div className={styles.add_mar_top_small}>
        <label>
          <input
            checked={isCascadeChecked}
            className={`${styles.add_mar_right_small} ${styles.cursorPointer}`}
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
        <OverlayTrigger placement="right" overlay={cascadeTip}>
          <i
            className={`${styles.add_mar_left_small} fa fa-info-circle`}
            aria-hidden="true"
          />
        </OverlayTrigger>
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
        <label>
          <input
            checked={isTableTrackChecked}
            className={`${styles.add_mar_right_small} ${styles.cursorPointer}`}
            id="track-checkbox"
            type="checkbox"
            onChange={dispatchTrackThis}
            data-test="raw-sql-track-check"
          />
          Track this
        </label>
        <OverlayTrigger placement="right" overlay={trackTableTip()}>
          <i
            className={`${styles.add_mar_left_small} fa fa-info-circle`}
            aria-hidden="true"
          />
        </OverlayTrigger>
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
              className={styles.add_mar_right_small}
              id="migration-checkbox"
              type="checkbox"
              onChange={dispatchIsMigration}
              data-test="raw-sql-migration-check"
            />
            This is a migration
          </label>
          <OverlayTrigger placement="right" overlay={migrationTip}>
            <i
              className={`${styles.add_mar_left_small} fa fa-info-circle`}
              aria-hidden="true"
            />
          </OverlayTrigger>
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
              <OverlayTrigger placement="right" overlay={migrationNameTip}>
                <i
                  className={`${styles.add_mar_left_small} fa fa-info-circle`}
                  aria-hidden="true"
                />
              </OverlayTrigger>
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

  function getStatementTimeoutSection() {
    const dispatchSetStatementTimeout = value => {
      const timeoutInSeconds = Number(value.trim());

      isNaN(timeoutInSeconds) || timeoutInSeconds <= 0
        ? dispatch(setRawSqlTimeout(0))
        : dispatch(setRawSqlTimeout(timeoutInSeconds));
    };
    return (
      <div className={styles.add_mar_top}>
        <label>
          Statement timeout(seconds)
          <OverlayTrigger placement="right" overlay={statementTimeoutTip}>
            <i
              className={`${styles.add_mar_left_small} fa fa-info-circle`}
              aria-hidden="true"
            />
          </OverlayTrigger>
          <input
            min={0}
            type="number"
            className={`${styles.inline_block} ${styles.add_mar_left_small}`}
            data-test="raw-sql-statement-timeout"
            onChange={event => dispatchSetStatementTimeout(event.target.value)}
          />
        </label>
      </div>
    );
  }

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
          <NotesSection />
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
          {getStatementTimeoutSection()}
          <Button
            type="submit"
            className={styles.add_mar_top}
            onClick={submitSQL}
            color="yellow"
            size="sm"
            data-test="run-sql"
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

      <div className={styles.add_mar_bottom}>{getResultTable()}</div>
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
};

const mapStateToProps = state => ({
  ...state.rawSQL,
  migrationMode: state.main.migrationMode,
  currentSchema: state.tables.currentSchema,
  allSchemas: state.tables.allSchemas,
  serverVersion: state.main.serverVersion ? state.main.serverVersion : '',
});

const rawSQLConnector = connect => connect(mapStateToProps)(RawSQL);

export default rawSQLConnector;
