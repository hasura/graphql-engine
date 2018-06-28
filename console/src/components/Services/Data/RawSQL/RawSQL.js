import React from 'react';
import PropTypes from 'prop-types';
import Helmet from 'react-helmet';
import AceEditor from 'react-ace';
import 'brace/mode/sql';
import Modal from 'react-bootstrap/lib/Modal';
import Button from 'react-bootstrap/lib/Button';
import { Link } from 'react-router';

import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import {
  executeSQL,
  SET_SQL,
  SET_MIGRATION_CHECKED,
  SET_TRACK_TABLE_CHECKED,
} from './Actions';
import { modalOpen, modalClose } from './Actions';
import globals from '../../../../Globals';

const migrationTip = (
  <Tooltip id="tooltip-migration">
    Any modifications to schema are recommended to be part of migrations.
  </Tooltip>
);
const trackTableTip = (
  <Tooltip id="tooltip-tracktable">
    If you are creating a table/view, you can track them to query them using
    APIs.
  </Tooltip>
);

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
  isMigrationChecked,
  isTableTrackChecked,
  migrationMode,
  currentSchema,
}) => {
  const styles = require('../TableCommon/Table.scss');

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

  const onModalClose = () => {
    dispatch(modalClose());
  };

  const onConfirmNoMigration = () => {
    const isMigration = document.getElementById('migration-checkbox').checked;
    dispatch(modalClose());
    dispatch(executeSQL(isMigration));
  };

  const resultTable = (() => {
    const tableHeadings = resultHeaders.map((columnName, i) => (
      <th key={i}>{columnName}</th>
    ));
    const rows = result.map((row, i) => (
      <tr key={i}>
        {row.map((columnValue, j) => <td key={j}>{columnValue}</td>)}
      </tr>
    ));
    return !resultType || resultType === 'command' ? null : (
      <div
        className={`${styles.addCol} col-xs-12 ${styles.padd_left_remove} ${
          styles.padd_top
        }`}
      >
        <h4 className={styles.subheading_text}>SQL Result:</h4>
        {/* Table for the results */}
        <div className={styles.tableContainer}>
          <table
            className={`${
              styles.table
            } table table-bordered table-striped table-hover`}
          >
            <thead>
              <tr>{tableHeadings}</tr>
            </thead>
            <tbody>{rows}</tbody>
          </table>
        </div>
        <br />
        <br />
      </div>
    );
  })();
  return (
    <div
      className={`${styles.main_wrapper} ${styles.padd_left} ${
        styles.padd_top
      }`}
    >
      <Helmet title="Run SQL - Data | Hasura" />
      <div className={styles.subHeader}>
        <h2 className={`${styles.heading_text} ${styles.remove_pad_bottom}`}>
          Raw SQL
        </h2>
        <div className="clearfix" />
      </div>
      <hr />
      <div>
        <div className={`${styles.addCol} col-xs-8 ${styles.padd_left_remove}`}>
          <div>
            <b>Notes</b>
            <ul className={styles.remove_ul_left}>
              <li>
                You can create views, alter tables or just use any SQL syntax to
                communicate with the database.
              </li>
              <li>
                If you plan to create a Table/View using Raw SQL, remember to
                link it to Hasura DB using&nbsp;
                <Link
                  to={
                    '/data/schema/' + currentSchema + '/existing-table-view/add'
                  }
                >
                  Add Existing Table View
                </Link>{' '}
                functionality.
              </li>
            </ul>
          </div>
          <hr />
          <h4>SQL:</h4>
          <AceEditor
            data-test="sql-test-editor"
            mode="sql"
            theme="github"
            name="raw_sql"
            value={sql}
            minLines={8}
            maxLines={100}
            width="100%"
            showPrintMargin={false}
            onChange={val => {
              dispatch({ type: SET_SQL, data: val });
              const formattedSql = val.toLowerCase();
              // set migration checkbox true
              if (
                formattedSql.indexOf('create') !== -1 ||
                formattedSql.indexOf('alter') !== -1 ||
                formattedSql.indexOf('drop') !== -1
              ) {
                dispatch({ type: SET_MIGRATION_CHECKED, data: true });
              } else {
                dispatch({ type: SET_MIGRATION_CHECKED, data: false });
              }
              // set track table checkbox true
              if (
                formattedSql.indexOf('create view') !== -1 ||
                formattedSql.indexOf('create table') !== -1
              ) {
                dispatch({ type: SET_TRACK_TABLE_CHECKED, data: true });
              } else {
                dispatch({ type: SET_TRACK_TABLE_CHECKED, data: false });
              }
            }}
          />
          <hr />
          {migrationMode && globals.consoleMode === 'cli' ? (
            <div>
              <input
                checked={isMigrationChecked}
                className={styles.add_mar_right_small}
                id="migration-checkbox"
                type="checkbox"
                onChange={() => {
                  dispatch({
                    type: SET_MIGRATION_CHECKED,
                    data: !isMigrationChecked,
                  });
                }}
              />
              This is a migration
              <OverlayTrigger placement="right" overlay={migrationTip}>
                <i
                  className={`${styles.padd_small_left} fa fa-info-circle`}
                  aria-hidden="true"
                />
              </OverlayTrigger>
              <div className={styles.padd_top_medium}>
                <input
                  checked={isTableTrackChecked}
                  className={styles.add_mar_right_small}
                  id="track-checkbox"
                  type="checkbox"
                  onChange={() => {
                    dispatch({
                      type: SET_TRACK_TABLE_CHECKED,
                      data: !isTableTrackChecked,
                    });
                  }}
                />
                Track table
                <OverlayTrigger placement="right" overlay={trackTableTip}>
                  <i
                    className={`${styles.padd_small_left} fa fa-info-circle`}
                    aria-hidden="true"
                  />
                </OverlayTrigger>
              </div>
              <hr />
            </div>
          ) : null}
          <button
            type="submit"
            className={styles.yellow_button}
            onClick={() => {
              // check migration mode global
              if (migrationMode) {
                const isMigration = document.getElementById(
                  'migration-checkbox'
                ).checked;
                if (!isMigration) {
                  // if migration is not checked, check if the sql text has any of 'create', 'alter', 'drop'
                  const formattedSql = sql.toLowerCase();
                  if (
                    formattedSql.indexOf('create') !== -1 ||
                    formattedSql.indexOf('alter') !== -1 ||
                    formattedSql.indexOf('drop') !== -1
                  ) {
                    // const confirmation = window.confirm('Your SQL Statement has a schema modifying command. Are you sure its not a migration?');
                    dispatch(modalOpen());
                    const confirmation = false;
                    if (confirmation) {
                      dispatch(executeSQL(isMigration));
                    }
                  } else {
                    dispatch(executeSQL(isMigration));
                  }
                } else {
                  dispatch(executeSQL(isMigration));
                }
              } else {
                dispatch(executeSQL(false));
              }
            }}
          >
            Run!
          </button>
        </div>
        <div className="hidden col-xs-4">{alert}</div>
      </div>
      <Modal show={isModalOpen} onHide={onModalClose.bind(this)}>
        <Modal.Header closeButton>
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
          <Button onClick={onModalClose}>Cancel</Button>
          <Button onClick={onConfirmNoMigration} bsStyle="primary">
            Yes, i confirm
          </Button>
        </Modal.Footer>
      </Modal>
      <div className={`${styles.padd_left_remove} container-fluid`}>
        {resultTable}
      </div>
    </div>
  );
};

RawSQL.propTypes = {
  sql: PropTypes.string.isRequired,
  resultType: PropTypes.string.isRequired,
  result: PropTypes.array.isRequired,
  resultHeaders: PropTypes.array.isRequired,
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
});

const rawSQLConnector = connect => connect(mapStateToProps)(RawSQL);

export default rawSQLConnector;
