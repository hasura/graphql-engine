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
import Button from '../../../Common/Button/Button';
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
import {
  autoAddRelName,
  autoTrackRelations,
  getAllUnTrackedRelations,
} from '../TableRelationships/Actions';
import globals from '../../../../Globals';
import { getRelDef } from '../TableRelationships/Relationships';

const appPrefix = globals.urlPrefix + '/data';

class Schema extends Component {
  constructor(props) {
    super(props);

    this.state = {
      isExporting: false,
    };

    // Initialize this table
    this.props.dispatch(fetchDataInit());
    this.props.dispatch(fetchFunctionInit());

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
      untrackedTables,
      migrationMode,
      untrackedRelations,
      currentSchema,
      dispatch,
      functionsList,
      // nonTrackableFunctionsList, // Not used right now, will be used in future
      trackedFunctions,
    } = this.props;

    const styles = require('../../../Common/Layout/LeftSubSidebar/LeftSubSidebar.scss');

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

    /***********/

    const getTrackableFunctions = () => {
      const trackedFuncNames = trackedFunctions.map(t => t.function_name);

      // Assuming schema for both function and tables are same
      // return function which are tracked && function name whose
      // set of tables are tracked
      const filterCondition = func => {
        return (
          !trackedFuncNames.includes(func.function_name) &&
          !!func.return_table_info
        );
      };

      return functionsList.filter(filterCondition);
    };

    const getUntrackedTables = () => {
      const tableNames = schema.map(item => item.table_name);
      const untrackedTableNames = untrackedTables.map(item => item.table_name);

      const schemaUntrackedTables = schema.filter(
        table => !untrackedTableNames.includes(table.table_name)
      );

      const untrackedTablesNotInSchema = untrackedTables.filter(
        table => !tableNames.includes(table.table_name)
      );

      const tableSortFunc = (a, b) => {
        return a.table_name === b.table_name
          ? 0
          : +(a.table_name > b.table_name) || -1;
      };

      const _untrackedTables = schemaUntrackedTables.concat(
        untrackedTablesNotInSchema
      );

      return _untrackedTables.sort(tableSortFunc);
    };

    /***********/

    const allUntrackedTables = getUntrackedTables();
    const trackableFuncs = getTrackableFunctions();

    const getCreateBtn = () => {
      let createBtn = null;

      if (migrationMode) {
        const handleClick = e => {
          e.preventDefault();

          dispatch(push(`${appPrefix}/schema/${currentSchema}/table/add`));
        };

        createBtn = (
          <Button
            data-test="data-create-table"
            color="yellow"
            size="sm"
            className={styles.add_mar_left}
            onClick={handleClick}
          >
            Create Table
          </Button>
        );
      }

      return createBtn;
    };

    const getCurrentSchemaSection = () => {
      const schemaOptions = schemaList.map(s => {
        return <option key={s.schema_name}>{s.schema_name}</option>;
      });

      return (
        <div className={styles.add_mar_top}>
          <div className={styles.display_inline}>Current Postgres schema</div>
          <div className={styles.display_inline}>
            <select
              onChange={handleSchemaChange}
              className={styles.changeSchema + ' form-control'}
              value={currentSchema}
            >
              {schemaOptions}
            </select>
          </div>
        </div>
      );
    };

    const getUntrackedTablesSection = () => {
      const getTrackAllBtn = () => {
        let trackAllBtn = null;

        if (allUntrackedTables.length > 0) {
          trackAllBtn = (
            <Button
              className={`${styles.display_inline} ${styles.addAllBtn}`}
              color="white"
              size="xs"
              onClick={e => {
                e.preventDefault();
                dispatch(addAllUntrackedTablesSql(allUntrackedTables));
              }}
            >
              Track All
            </Button>
          );
        }

        return trackAllBtn;
      };

      const getUntrackedTablesList = () => {
        const untrackedTablesList = [];

        allUntrackedTables.forEach((table, i) => {
          const handleTrackTable = e => {
            e.preventDefault();

            dispatch(setTableName(table.table_name));
            dispatch(addExistingTableSql());
          };

          untrackedTablesList.push(
            <div className={styles.padd_bottom} key={`untracked-${i}`}>
              <div className={styles.inline_block}>
                <Button
                  data-test={`add-track-table-${table.table_name}`}
                  className={`${styles.display_inline}`}
                  color="white"
                  size="xs"
                  onClick={handleTrackTable}
                >
                  Track
                </Button>
              </div>
              <div className={styles.inline_block}>{table.table_name}</div>
            </div>
          );
        });

        if (!untrackedTablesList.length) {
          untrackedTablesList.push(
            <div key="no-untracked">There are no untracked tables or views</div>
          );
        }

        return untrackedTablesList;
      };

      return (
        <div className={styles.add_mar_top}>
          <div>
            <h4
              className={`${styles.subheading_text} ${styles.heading_tooltip}`}
            >
              Untracked tables or views
            </h4>
            <OverlayTrigger placement="right" overlay={untrackedTip}>
              <i className="fa fa-info-circle" aria-hidden="true" />
            </OverlayTrigger>
            {getTrackAllBtn()}
          </div>
          <div className={`${styles.padd_left_remove} col-xs-12`}>
            {getUntrackedTablesList()}
          </div>
          <div className={styles.clear_fix} />
        </div>
      );
    };

    const getUntrackedRelationsSection = () => {
      const getTrackAllBtn = () => {
        let trackAllBtn = null;

        const trackAllRelations = () => {
          this.props.dispatch(autoTrackRelations(untrackedRelations));
        };

        if (untrackedRelations.length > 0) {
          trackAllBtn = (
            <Button
              onClick={trackAllRelations}
              className={`${styles.display_inline} ${styles.add_mar_left}`}
              color="white"
              size="xs"
              data-test="track-all-relationships"
            >
              Track All
            </Button>
          );
        }

        return trackAllBtn;
      };

      const getUntrackedRelList = () => {
        const untrackedRelList = [];

        untrackedRelations.forEach((rel, i) => {
          const relData = rel.data;

          const handleAddRel = e => {
            e.preventDefault();

            dispatch(autoAddRelName(rel));
          };

          const relFrom = <b>{relData.tableName}</b>;

          const relTo = relData.isObjRel ? (
            <b>{relData.rTable}</b>
          ) : (
            <b>[ {relData.rTable} ]</b>
          );

          untrackedRelList.push(
            <div className={styles.padd_bottom} key={`untracked-rel-${i}`}>
              <div className={styles.inline_block}>
                <Button
                  className={styles.display_inline}
                  color="white"
                  size="xs"
                  onClick={handleAddRel}
                >
                  Track
                </Button>
              </div>
              <div className={styles.inline_block}>
                <span>
                  {relFrom} &rarr; {relTo}
                </span>
                &nbsp;&nbsp; - &nbsp;&nbsp;
                <span>
                  {getRelDef(
                    relData.isObjRel,
                    relData.lcol,
                    relData.rcol,
                    relData.tableName,
                    relData.rTable
                  )}
                </span>
              </div>
            </div>
          );
        });

        if (!untrackedRelList.length) {
          untrackedRelList.push(
            <div key="no-untracked-rel">There are no untracked relations</div>
          );
        }

        return untrackedRelList;
      };

      return (
        <div className={styles.add_mar_top}>
          <div>
            <h4
              className={`${styles.subheading_text} ${styles.heading_tooltip}`}
            >
              Untracked foreign-key relations
            </h4>
            <OverlayTrigger placement="right" overlay={untrackedRelTip}>
              <i className="fa fa-info-circle" aria-hidden="true" />
            </OverlayTrigger>
            {getTrackAllBtn()}
          </div>
          <div className={`${styles.padd_left_remove} col-xs-12`}>
            {getUntrackedRelList()}
          </div>
          <div className={styles.clear_fix} />
        </div>
      );
    };

    const getUntrackedFunctionsSection = () => {
      let trackableFunctionList = null;

      if (trackableFuncs.length > 0) {
        trackableFunctionList = (
          <div className={styles.add_mar_top} key={'custom-functions-content'}>
            <div>
              <h4
                className={`${styles.subheading_text} ${
                  styles.heading_tooltip
                }`}
              >
                Untracked custom functions
              </h4>
              <OverlayTrigger placement="right" overlay={trackableFunctions}>
                <i className="fa fa-info-circle" aria-hidden="true" />
              </OverlayTrigger>
            </div>
            <div className={`${styles.padd_left_remove} col-xs-12`}>
              {trackableFuncs.map((p, i) => (
                <div
                  className={styles.padd_bottom}
                  key={`${i}untracked-function`}
                >
                  <div
                    className={`${styles.display_inline} ${styles.padd_right}`}
                  >
                    <Button
                      data-test={`add-track-function-${p.function_name}`}
                      className={`${
                        styles.display_inline
                      } btn btn-xs btn-default`}
                      onClick={e => {
                        e.preventDefault();
                        dispatch(addExistingFunction(p.function_name));
                      }}
                    >
                      Track
                    </Button>
                  </div>
                  <div
                    className={`${styles.padd_right} ${styles.inline_block}`}
                  >
                    {p.function_name}
                  </div>
                </div>
              ))}
            </div>
            <div className={styles.clear_fix} />
          </div>
        );
      }

      return trackableFunctionList;
    };

    const getNonTrackableFunctionsSection = () => {
      const nonTrackableFuncList = null;

      // if (nonTrackableFunctionsList.length > 0) {
      //   nonTrackableFuncList = (
      //     <div
      //       className={styles.add_mar_top}
      //       key={'non-trackable-custom-functions-content'}
      //     >
      //       <div>
      //         <h4
      //           className={`${styles.subheading_text} ${
      //             styles.heading_tooltip
      //           }`}
      //         >
      //         Non trackable custom functions
      //         </h4>
      //         <OverlayTrigger
      //           placement="right"
      //           overlay={nonTrackableFunctions}
      //         >
      //           <i className="fa fa-info-circle" aria-hidden="true" />
      //         </OverlayTrigger>
      //       </div>
      //       <div className={`${styles.padd_left_remove} col-xs-12`}>
      //         {nonTrackableFunctionsList.map((p, i) => (
      //           <div
      //             className={styles.padd_bottom}
      //             key={`${i}untracked-function`}
      //           >
      //             <div
      //               className={`${styles.padd_right} ${
      //                 styles.inline_block
      //               }`}
      //             >
      //               {p.function_name}
      //             </div>
      //           </div>
      //         ))}
      //       </div>
      //       <div className={styles.clear_fix} />
      //     </div>
      //   );
      // }

      return nonTrackableFuncList;
    };

    return (
      <div
        className={`container-fluid ${styles.padd_left_remove} ${
          styles.padd_top
        }`}
      >
        <div className={styles.padd_left}>
          <Helmet title="Schema - Data | Hasura" />
          <div className={styles.display_flex}>
            <h2 className={`${styles.headerText} ${styles.inline_block}`}>
              Schema
            </h2>
            {getCreateBtn()}
          </div>
          {getCurrentSchemaSection()}
          {getUntrackedTablesSection()}
          {getUntrackedRelationsSection()}
          {getUntrackedFunctionsSection()}
          {getNonTrackableFunctionsSection()}
        </div>
      </div>
    );
  }
}

Schema.propTypes = {
  schema: PropTypes.array.isRequired,
  untrackedTables: PropTypes.array.isRequired,
  untrackedRelations: PropTypes.array.isRequired,
  migrationMode: PropTypes.bool.isRequired,
  currentSchema: PropTypes.string.isRequired,
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = state => ({
  schema: state.tables.allSchemas,
  schemaList: state.tables.schemaList,
  untrackedTables: state.tables.untrackedSchemas,
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
