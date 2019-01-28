/* eslint-disable space-infix-ops */
/* eslint-disable no-loop-func  */

import PropTypes from 'prop-types';

import React, { Component } from 'react';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';

import {
  untrackedTip,
  untrackedRelTip,
  trackableFunctions,
  // nonTrackableFunctions,
} from './Tooltips';
import Button from '../../Layout/Button/Button';
import {
  setTableName,
  addExistingTableSql,
  addAllUntrackedTablesSql,
  addExistingFunction,
} from '../Add/AddExistingTableViewActions';
import {
  loadUntrackedRelations,
  fetchDataInit,
  fetchFunctionInit,
  LOAD_UNTRACKED_RELATIONS,
  UPDATE_CURRENT_SCHEMA,
} from '../DataActions';
import { getAllUnTrackedRelations } from '../TableRelationships/Actions';
import AutoAddRelationsConnector from './AutoAddRelations';
import globals from '../../../../Globals';

const appPrefix = globals.urlPrefix + '/data';

class Schema extends Component {
  constructor(props) {
    super(props);
    this.state = {
      isExporting: false,
    };
    // Initialize this table
    const dispatch = this.props.dispatch;
    dispatch(fetchDataInit());
    dispatch(fetchFunctionInit());
    const untrackedRelations = getAllUnTrackedRelations(
      this.props.schema,
      this.props.currentSchema
    ).bulkRelTrack;
    this.props.dispatch({
      type: LOAD_UNTRACKED_RELATIONS,
      untrackedRelations,
    });
  }

  componentDidMount() {
    const untrackedRelations = getAllUnTrackedRelations(
      this.props.schema,
      this.props.currentSchema
    ).bulkRelTrack;
    this.props.dispatch({
      type: LOAD_UNTRACKED_RELATIONS,
      untrackedRelations,
    });
  }

  render() {
    const {
      schema,
      schemaList,
      untracked,
      migrationMode,
      untrackedRelations,
      currentSchema,
      dispatch,
      functionsList,
      // nonTrackableFunctionsList, // Not used right now, will be used in future
      trackedFunctions,
    } = this.props;

    /* Filter */
    const trackedFuncs = trackedFunctions.map(t => t.function_name);
    // Assuming schema for both function and tables are same
    const trackableFuncs = functionsList.filter(f => {
      // return function which are tracked && function name whose setof tables are tracked
      return (
        trackedFuncs.indexOf(f.function_name) === -1 && !!f.return_table_info
      ); // && add condition which will check whether the setoff table is tracked or not
    });
    /* */

    const handleSchemaChange = e => {
      const updatedSchema = e.target.value;
      dispatch(push(`${appPrefix}/schema/${updatedSchema}`));
      Promise.all([
        dispatch({ type: UPDATE_CURRENT_SCHEMA, currentSchema: updatedSchema }),
        dispatch(fetchDataInit()),
        dispatch(fetchFunctionInit()),
        dispatch(loadUntrackedRelations()),
      ]);
    };

    const styles = require('../PageContainer/PageContainer.scss');
    let relationships = 0;
    schema.map(t => (relationships += t.relationships.length));

    // find which tables are untracked
    const ids1 = schema.map(item => item.table_name);
    const ids2 = untracked.map(item => item.table_name);

    const untrackedTables = ids1
      .map((id, index) => {
        if (ids2.indexOf(id) < 0) {
          return schema[index];
        }
      })
      .concat(
        ids2.map((id, index) => {
          if (ids1.indexOf(id) < 0) {
            return untracked[index];
          }
        })
      )
      .filter(item => item !== undefined)
      .sort((a, b) => {
        return a.table_name === b.table_name
          ? 0
          : +(a.table_name > b.table_name) || -1;
      });

    const untrackedHtml = [];
    for (let i = 0; i < untrackedTables.length; i++) {
      untrackedHtml.push(
        <div className={styles.padd_bottom} key={`${i}untracked`}>
          <div className={`${styles.display_inline} ${styles.padd_right}`}>
            <Button
              data-test={`add-track-table-${untrackedTables[i].table_name}`}
              className={`${styles.display_inline}`}
              color="white"
              size="xs"
              onClick={e => {
                e.preventDefault();
                dispatch(setTableName(untrackedTables[i].table_name));
                dispatch(addExistingTableSql());
              }}
            >
              Add
            </Button>
          </div>
          <div className={`${styles.padd_right} ${styles.inline_block}`}>
            {untrackedTables[i].table_name}
          </div>
        </div>
      );
    }
    if (!untrackedHtml.length) {
      untrackedHtml.push(
        <div key="no-untracked">There are no untracked tables or views</div>
      );
    }

    return (
      <div
        className={`${styles.padd_left_remove} container-fluid ${
          styles.padd_top
        }`}
      >
        <div className={styles.padd_left}>
          <Helmet title="Schema - Data | Hasura" />
          <div>
            <h2 className={`${styles.heading_text} ${styles.inline_block}`}>
              {' '}
              Schema{' '}
            </h2>
            {migrationMode ? (
              <Button
                data-test="data-create-table"
                color="yellow"
                size="sm"
                onClick={e => {
                  e.preventDefault();
                  dispatch(
                    push(`${appPrefix}/schema/${currentSchema}/table/add`)
                  );
                }}
              >
                Create Table
              </Button>
            ) : null}
          </div>
          <hr />
          <div>
            <div className={styles.display_inline}>Current postgres schema</div>
            <div className={styles.display_inline}>
              <select
                onChange={handleSchemaChange}
                className={styles.changeSchema + ' form-control'}
                value={currentSchema}
              >
                {schemaList.map(s => {
                  if (s.schema_name === currentSchema) {
                    return <option key={s.schema_name}>{s.schema_name}</option>;
                  }
                  return <option key={s.schema_name}>{s.schema_name}</option>;
                })}
              </select>
            </div>
          </div>
          <hr />
          <div className={styles.add_pad_bottom}>
            <div>
              <h4
                className={`${styles.subheading_text} ${
                  styles.heading_tooltip
                }`}
              >
                Untracked tables or views
              </h4>
              <OverlayTrigger placement="right" overlay={untrackedTip}>
                <i className="fa fa-info-circle" aria-hidden="true" />
              </OverlayTrigger>
              {untrackedTables.length > 0 ? (
                <Button
                  className={`${styles.display_inline} ${styles.addAllBtn}`}
                  color="white"
                  size="xs"
                  onClick={e => {
                    e.preventDefault();
                    dispatch(addAllUntrackedTablesSql(untrackedTables));
                  }}
                >
                  Add all
                </Button>
              ) : null}
            </div>
            <div className={`${styles.padd_left_remove} col-xs-12`}>
              {untrackedHtml}
            </div>
          </div>
          <hr />
          <div className={styles.wd100 + ' ' + styles.clear_fix}>
            <h4
              className={`${styles.subheading_text} ${styles.heading_tooltip}`}
            >
              Untracked foreign-key relations
            </h4>
            <OverlayTrigger placement="right" overlay={untrackedRelTip}>
              <i className="fa fa-info-circle" aria-hidden="true" />
            </OverlayTrigger>
            <div className={`${styles.padd_left_remove} col-xs-12`}>
              <div>
                <AutoAddRelationsConnector
                  untrackedRelations={untrackedRelations}
                  schema={schema}
                  dispatch={dispatch}
                />
              </div>
            </div>
          </div>

          {trackableFuncs.length > 0
            ? [
              <hr
                className={styles.wd100 + ' ' + styles.clear_fix}
                key={'custom-functions-hr'}
              />,
              <div
                className={styles.wd100 + ' ' + styles.clear_fix}
                key={'custom-functions-content'}
              >
                <h4
                  className={`${styles.subheading_text} ${
                    styles.heading_tooltip
                  }`}
                >
                    Untracked custom functions
                </h4>
                <OverlayTrigger
                  placement="right"
                  overlay={trackableFunctions}
                >
                  <i className="fa fa-info-circle" aria-hidden="true" />
                </OverlayTrigger>
                <div className={`${styles.padd_left_remove} col-xs-12`}>
                  {trackableFuncs.map((p, i) => (
                    <div
                      className={styles.padd_bottom}
                      key={`${i}untracked-function`}
                    >
                      <div
                        className={`${styles.display_inline} ${
                          styles.padd_right
                        }`}
                      >
                        <button
                          data-test={`add-track-function-${p.function_name}`}
                          className={`${
                            styles.display_inline
                          } btn btn-xs btn-default`}
                          onClick={e => {
                            e.preventDefault();
                            dispatch(addExistingFunction(p.function_name));
                          }}
                        >
                            Add
                        </button>
                      </div>
                      <div
                        className={`${styles.padd_right} ${
                          styles.inline_block
                        }`}
                      >
                        {p.function_name}
                      </div>
                    </div>
                  ))}
                </div>
              </div>,
            ]
            : null}

          {/* nonTrackableFunctionsList.length > 0
            ? [
              <hr
                className={styles.wd100 + ' ' + styles.clear_fix}
                key={'non-trackable-custom-functions-id'}
              />,
              <div
                className={styles.wd100 + ' ' + styles.clear_fix}
                key={'non-trackable-custom-functions-content'}
              >
                <h4
                  className={`${styles.subheading_text} ${
                    styles.heading_tooltip
                  }`}
                >
                    Non trackable custom functions
                </h4>
                <OverlayTrigger
                  placement="right"
                  overlay={nonTrackableFunctions}
                >
                  <i className="fa fa-info-circle" aria-hidden="true" />
                </OverlayTrigger>
                <div className={`${styles.padd_left_remove} col-xs-12`}>
                  {nonTrackableFunctionsList.map((p, i) => (
                    <div
                      className={styles.padd_bottom}
                      key={`${i}untracked-function`}
                    >
                      <div
                        className={`${styles.padd_right} ${
                          styles.inline_block
                        }`}
                      >
                        {p.function_name}
                      </div>
                    </div>
                  ))}
                </div>
              </div>,
            ]
            : null */}
        </div>
      </div>
    );
  }
}

Schema.propTypes = {
  schema: PropTypes.array.isRequired,
  untracked: PropTypes.array.isRequired,
  untrackedRelations: PropTypes.array.isRequired,
  migrationMode: PropTypes.bool.isRequired,
  currentSchema: PropTypes.string.isRequired,
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = state => ({
  schema: state.tables.allSchemas,
  schemaList: state.tables.schemaList,
  untracked: state.tables.untrackedSchemas,
  migrationMode: state.main.migrationMode,
  untrackedRelations: state.tables.untrackedRelations,
  currentSchema: state.tables.currentSchema,
  functionsList: [...state.tables.postgresFunctions],
  nonTrackableFunctionsList: [...state.tables.nonTrackablePostgresFunctions],
  trackedFunctions: [...state.tables.trackedFunctions],
  serverVersion: state.main.serverVersion ? state.main.serverVersion : '',
});

const schemaConnector = connect => connect(mapStateToProps)(Schema);

export default schemaConnector;
