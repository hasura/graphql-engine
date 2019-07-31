import React from 'react';
import PropTypes from 'prop-types';
import Helmet from 'react-helmet';
import AceEditor from 'react-ace';
import 'brace/mode/sql';
import Modal from 'react-bootstrap/lib/Modal';
import ModalButton from 'react-bootstrap/lib/Button';
import Button from '../../../Common/Button/Button';
import { parseCreateSQL } from './utils';

import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import {
  executeSQL,
  SET_SQL,
  SET_CASCADE_CHECKED,
  SET_MIGRATION_CHECKED,
  SET_TRACK_TABLE_CHECKED,
} from './Actions';
import { modalOpen, modalClose } from './Actions';
import globals from '../../../../Globals';
import './AceEditorFix.css';

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

  const isSchemaModification = _sql => {
    const formattedSQL = _sql.toLowerCase();

    return (
      formattedSQL.includes('create') ||
      formattedSQL.includes('alter') ||
      formattedSQL.includes('drop')
    );
  };

  const submitSQL = () => {
    // check migration mode global
    if (migrationMode) {
      const checkboxElem = document.getElementById('migration-checkbox');
      const isMigration = checkboxElem ? checkboxElem.checked : false;
      const textboxElem = document.getElementById('migration-name');
      let migrationName = textboxElem ? textboxElem.value : '';
      if (isMigration && migrationName.length === 0) {
        migrationName = 'run_sql_migration';
      }
      if (!isMigration && globals.consoleMode === 'cli') {
        // if migration is not checked, check if is schema modification
        if (isSchemaModification(sql)) {
          // const confirmation = window.confirm('Your SQL Statement has a schema modifying command. Are you sure its not a migration?');
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

  let alert = null;

  if (ongoingRequest) {
    alert = (
      <div className={`${styles.padd_left_remove} col-xs-12`}>
        <div className="hidden alert alert-warning" role="alert">
          Running...
        </div>
      </div>
    );
  } else if (lastError) {
    alert = (
      <div className={`${styles.padd_left_remove} col-xs-12`}>
        <div className="hidden alert alert-danger" role="alert">
          Error: {JSON.stringify(lastError)}
        </div>
      </div>
    );
  } else if (lastSuccess) {
    alert = (
      <div className={`${styles.padd_left_remove} col-xs-12`}>
        <div className="hidden alert alert-success" role="alert">
          Executed Query
        </div>
      </div>
    );
  }

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
      <Modal show={isModalOpen} onHide={onModalClose.bind(this)}>
        <Modal.Header closeModalButton>
          <Modal.Title>Run SQL</Modal.Title>
        </Modal.Header>
        <Modal.Body>
          <div className="content-fluid">
            <div className="row">
              <div className="col-xs-12">
                Your SQL Statement is most likely modifying the database schema.
                Are you sure its not a migration?
              </div>
            </div>
          </div>
        </Modal.Body>
        <Modal.Footer>
          <ModalButton onClick={onModalClose}>Cancel</ModalButton>
          <ModalButton
            onClick={onConfirmNoMigration}
            bsStyle="primary"
            data-test="not-migration-confirm"
          >
            Yes, i confirm
          </ModalButton>
        </Modal.Footer>
      </Modal>
    );
  };

  const getSQLSection = () => {
    const handleSQLChange = val => {
      dispatch({ type: SET_SQL, data: val });

      // set migration checkbox true
      if (isSchemaModification(val)) {
        dispatch({ type: SET_MIGRATION_CHECKED, data: true });
      } else {
        dispatch({ type: SET_MIGRATION_CHECKED, data: false });
      }

      // set track this checkbox true
      const objects = parseCreateSQL(val, true);
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
          theme="github"
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
              className={`table table-bordered table-striped table-hover ${
                styles.table
              } `}
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

  const getNotesSection = () => {
    return (
      <ul>
        <li>
          You can create views, alter tables or just about run any SQL
          statements directly on the database.
        </li>
        <li>
          Multiple SQL statements can be separated by semicolons, <code>;</code>
          , however, only the result of the last SQL statement will be returned.
        </li>
        <li>
          Multiple SQL statements will be run as a transaction. i.e. if any
          statement fails, none of the statements will be applied.
        </li>
      </ul>
    );
  };

  const getMetadataCascadeSection = () => {
    return (
      <div className={styles.add_mar_top_small}>
        <input
          checked={isCascadeChecked}
          className={styles.add_mar_right_small}
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
            className={styles.add_mar_right_small}
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
          <input
            checked={isMigrationChecked}
            className={styles.add_mar_right_small}
            id="migration-checkbox"
            type="checkbox"
            onChange={dispatchIsMigration}
            data-test="raw-sql-migration-check"
          />
          This is a migration
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
                className={
                  styles.inline_block +
                  ' ' +
                  styles.tableNameInput +
                  ' ' +
                  styles.add_mar_right_small +
                  ' ' +
                  ' form-control'
                }
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

    if (migrationMode && globals.consoleMode === 'cli') {
      migrationSection = (
        <div className={styles.add_mar_top_small}>
          {getIsMigrationSection()}
          {getMigrationNameSection()}
        </div>
      );
    }

    return migrationSection;
  };

  const getRunButton = () => {
    return (
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
    );
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
        <div>
          <div className={`${styles.padd_left_remove} col-xs-8`}>
            {getNotesSection()}
          </div>

          <div className={`${styles.padd_left_remove} col-xs-10`}>
            {getSQLSection()}
          </div>

          <div
            className={`${styles.padd_left_remove} ${
              styles.add_mar_bottom
            } col-xs-8`}
          >
            {getTrackThisSection()}
            {getMetadataCascadeSection()}
            {getMigrationSection()}

            {getRunButton()}
          </div>
        </div>
        <div className="hidden col-xs-4">{alert}</div>
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
