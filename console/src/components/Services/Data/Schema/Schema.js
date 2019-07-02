import PropTypes from 'prop-types';

import React, { Component } from 'react';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';

import {
  untrackedTip,
  untrackedRelTip,
  trackableFunctionsTip,
  nonTrackableFunctionsTip,
} from './Tooltips';
import Button from '../../../Common/Button/Button';
import {
  setTableName,
  addExistingTableSql,
  addAllUntrackedTablesSql,
  addExistingFunction,
} from '../Add/AddExistingTableViewActions';
import {
  updateSchemaInfo,
  fetchFunctionInit,
  updateCurrentSchema,
} from '../DataActions';
import {
  autoAddRelName,
  autoTrackRelations,
} from '../TableRelationships/Actions';
import globals from '../../../../Globals';
import { getRelDef } from '../TableRelationships/utils';
import { createNewSchema, deleteCurrentSchema } from './Actions';
import CollapsibleToggle from '../../../Common/CollapsibleToggle/CollapsibleToggle';

const appPrefix = globals.urlPrefix + '/data';

class Schema extends Component {
  constructor(props) {
    super(props);

    this.state = {
      isExporting: false,
      createSchemaOpen: false,
      schemaNameEdit: '',
    };

    this.props.dispatch(fetchFunctionInit());
    this.props.dispatch(
      updateSchemaInfo({ schemas: [this.props.currentSchema] })
    );
  }

  render() {
    const {
      schema,
      schemaList,
      migrationMode,
      untrackedRelations,
      currentSchema,
      dispatch,
      functionsList,
      nonTrackableFunctions,
      trackedFunctions,
    } = this.props;

    const styles = require('../../../Common/Common.scss');

    const handleSchemaChange = e => {
      dispatch(updateCurrentSchema(e.target.value));
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
      const tableSortFunc = (a, b) => {
        return a.table_name === b.table_name
          ? 0
          : +(a.table_name > b.table_name) || -1;
      };

      const _untrackedTables = schema.filter(
        table => !table.is_table_tracked && table.table_schema === currentSchema
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
      const getSchemaOptions = () => {
        return schemaList.map(s => (
          <option key={s.schema_name}>{s.schema_name}</option>
        ));
      };

      const getCreateSchemaSection = () => {
        let createSchemaSection = null;

        if (migrationMode) {
          const { createSchemaOpen, schemaNameEdit } = this.state;

          const handleCreateNewClick = () => {
            this.setState({ createSchemaOpen: true });
          };

          const handleSchemaNameChange = e => {
            this.setState({ schemaNameEdit: e.target.value });
          };

          const handleCreateClick = () => {
            const schemaName = schemaNameEdit.trim();

            const successCb = () => {
              dispatch(updateCurrentSchema(schemaName));

              this.setState({
                schemaNameEdit: '',
                createSchemaOpen: false,
              });
            };

            dispatch(createNewSchema(schemaName, successCb));
          };

          const handleCancelCreateNewSchema = () => {
            this.setState({
              createSchemaOpen: false,
            });
          };

          const closedCreateSection = (
            <Button
              color="white"
              size="xs"
              onClick={handleCreateNewClick}
              title="Create new schema"
            >
              <i className="fa fa-plus" aria-hidden="true" />
            </Button>
          );

          const openCreateSection = (
            <div className={styles.display_inline + ' ' + styles.add_mar_left}>
              <div className={styles.display_inline}>
                <input
                  type="text"
                  value={schemaNameEdit}
                  onChange={handleSchemaNameChange}
                  placeholder="schema_name"
                  className={'form-control input-sm ' + styles.display_inline}
                />
              </div>
              <Button
                color="white"
                size="xs"
                onClick={handleCreateClick}
                className={styles.add_mar_left_mid}
              >
                Create
              </Button>
              <Button
                color="white"
                size="xs"
                onClick={handleCancelCreateNewSchema}
                className={styles.add_mar_left_mid}
              >
                Cancel
              </Button>
            </div>
          );

          createSchemaSection = createSchemaOpen
            ? openCreateSection
            : closedCreateSection;
        }

        return createSchemaSection;
      };

      const getDeleteSchemaBtn = () => {
        let deleteSchemaBtn = null;

        if (migrationMode) {
          const handleDelete = () => {
            const successCb = () => {
              dispatch(updateCurrentSchema('public'));
            };

            dispatch(deleteCurrentSchema(successCb));
          };

          deleteSchemaBtn = (
            <Button
              color="white"
              size="xs"
              onClick={handleDelete}
              title="Delete current schema"
            >
              <i className="fa fa-trash" aria-hidden="true" />
            </Button>
          );
        }

        return deleteSchemaBtn;
      };

      return (
        <div className={styles.add_mar_top}>
          <div className={styles.display_inline}>Current Postgres schema</div>
          <div className={styles.display_inline}>
            <select
              onChange={handleSchemaChange}
              className={
                styles.add_mar_left_mid +
                ' ' +
                styles.width_auto +
                ' form-control'
              }
              value={currentSchema}
            >
              {getSchemaOptions()}
            </select>
          </div>
          <div className={styles.display_inline + ' ' + styles.add_mar_left}>
            <div className={styles.display_inline}>{getDeleteSchemaBtn()}</div>
            <div
              className={`${styles.display_inline} ${styles.add_mar_left_mid}`}
            >
              {getCreateSchemaSection()}
            </div>
          </div>
        </div>
      );
    };

    const getUntrackedTablesSection = () => {
      const getTrackAllBtn = () => {
        let trackAllBtn = null;

        const trackAllTables = e => {
          e.stopPropagation();
          e.preventDefault();

          dispatch(addAllUntrackedTablesSql(allUntrackedTables));
        };

        if (allUntrackedTables.length > 0) {
          trackAllBtn = (
            <Button
              className={`${styles.display_inline} ${styles.add_mar_left}`}
              color="white"
              size="xs"
              onClick={trackAllTables}
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
              <div
                className={`${styles.display_inline} ${styles.add_mar_right}`}
              >
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
              <div className={styles.display_inline}>{table.table_name}</div>
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

      const heading = (
        <div>
          <h4
            className={`${styles.subheading_text} ${styles.display_inline} ${
              styles.add_mar_right_mid
            }`}
          >
            Untracked tables or views
          </h4>
          <OverlayTrigger placement="right" overlay={untrackedTip}>
            <i className="fa fa-info-circle" aria-hidden="true" />
          </OverlayTrigger>
          {getTrackAllBtn()}
        </div>
      );

      return (
        <div className={styles.add_mar_top}>
          <CollapsibleToggle title={heading} isOpen>
            <div className={`${styles.padd_left_remove} col-xs-12`}>
              {getUntrackedTablesList()}
            </div>
            <div className={styles.clear_fix} />
          </CollapsibleToggle>
        </div>
      );
    };

    const getUntrackedRelationsSection = () => {
      const getTrackAllBtn = () => {
        let trackAllBtn = null;

        const trackAllRelations = e => {
          e.stopPropagation();
          e.preventDefault();

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

          const relFrom = <b>{relData.lTable}</b>;

          const relTo = relData.isObjRel ? (
            <b>{relData.rTable}</b>
          ) : (
            <b>[ {relData.rTable} ]</b>
          );

          untrackedRelList.push(
            <div className={styles.padd_bottom} key={`untracked-rel-${i}`}>
              <div
                className={`${styles.display_inline} ${styles.add_mar_right}`}
              >
                <Button
                  className={styles.display_inline}
                  color="white"
                  size="xs"
                  onClick={handleAddRel}
                >
                  Track
                </Button>
              </div>
              <div className={styles.display_inline}>
                <span>
                  {relFrom} &rarr; {relTo}
                </span>
                &nbsp;&nbsp; - &nbsp;&nbsp;
                <span>{getRelDef(relData)}</span>
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

      const heading = (
        <div>
          <h4
            className={`${styles.subheading_text} ${styles.display_inline} ${
              styles.add_mar_right_mid
            }`}
          >
            Untracked foreign-key relations
          </h4>
          <OverlayTrigger placement="right" overlay={untrackedRelTip}>
            <i className="fa fa-info-circle" aria-hidden="true" />
          </OverlayTrigger>
          {getTrackAllBtn()}
        </div>
      );

      return (
        <div className={styles.add_mar_top}>
          <CollapsibleToggle title={heading} isOpen>
            <div className={`${styles.padd_left_remove} col-xs-12`}>
              {getUntrackedRelList()}
            </div>
            <div className={styles.clear_fix} />
          </CollapsibleToggle>
        </div>
      );
    };

    const getUntrackedFunctionsSection = () => {
      let trackableFunctionList = null;

      if (trackableFuncs.length > 0) {
        const heading = (
          <div>
            <h4
              className={`${styles.subheading_text} ${styles.display_inline} ${
                styles.add_mar_right_mid
              }`}
            >
              Untracked custom functions
            </h4>
            <OverlayTrigger placement="right" overlay={trackableFunctionsTip}>
              <i className="fa fa-info-circle" aria-hidden="true" />
            </OverlayTrigger>
          </div>
        );

        trackableFunctionList = (
          <div className={styles.add_mar_top} key={'custom-functions-content'}>
            <CollapsibleToggle title={heading} isOpen>
              <div className={`${styles.padd_left_remove} col-xs-12`}>
                {trackableFuncs.map((p, i) => (
                  <div
                    className={styles.padd_bottom}
                    key={`${i}untracked-function`}
                  >
                    <div
                      className={`${styles.display_inline} ${
                        styles.add_mar_right
                      }`}
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
                    <div className={styles.display_inline}>
                      <span>{p.function_name}</span>
                    </div>
                  </div>
                ))}
              </div>
              <div className={styles.clear_fix} />
            </CollapsibleToggle>
          </div>
        );
      }

      return trackableFunctionList;
    };

    const getNonTrackableFunctionsSection = () => {
      let nonTrackableFuncList = null;

      if (nonTrackableFunctions.length > 0) {
        const heading = (
          <div>
            <h4
              className={`${styles.subheading_text} ${styles.display_inline} ${
                styles.add_mar_right_mid
              }`}
            >
              Non trackable custom functions
            </h4>
            <OverlayTrigger
              placement="right"
              overlay={nonTrackableFunctionsTip}
            >
              <i className="fa fa-info-circle" aria-hidden="true" />
            </OverlayTrigger>
          </div>
        );

        nonTrackableFuncList = (
          <div
            className={styles.add_mar_top}
            key={'non-trackable-custom-functions'}
          >
            <CollapsibleToggle title={heading} isOpen>
              <div className={`${styles.padd_left_remove} col-xs-12`}>
                {nonTrackableFunctions.map((p, i) => (
                  <div
                    className={styles.padd_bottom}
                    key={`${i}untracked-function`}
                  >
                    <div
                      className={`${styles.padd_right} ${
                        styles.display_inline
                      }`}
                    >
                      {p.function_name}
                    </div>
                  </div>
                ))}
              </div>
              <div className={styles.clear_fix} />
            </CollapsibleToggle>
          </div>
        );
      }

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
            <h2 className={`${styles.headerText} ${styles.display_inline}`}>
              Schema
            </h2>
            {getCreateBtn()}
          </div>
          {getCurrentSchemaSection()}
          {getUntrackedTablesSection()}
          {getUntrackedRelationsSection()}
          {getUntrackedFunctionsSection()}
          {false && getNonTrackableFunctionsSection()}
        </div>
      </div>
    );
  }
}

Schema.propTypes = {
  schema: PropTypes.array.isRequired,
  untrackedRelations: PropTypes.array.isRequired,
  migrationMode: PropTypes.bool.isRequired,
  currentSchema: PropTypes.string.isRequired,
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = state => ({
  schema: state.tables.allSchemas,
  schemaList: state.tables.schemaList,
  migrationMode: state.main.migrationMode,
  untrackedRelations: state.tables.untrackedRelations,
  currentSchema: state.tables.currentSchema,
  functionsList: [...state.tables.postgresFunctions],
  nonTrackableFunctions: [...state.tables.nonTrackablePostgresFunctions],
  trackedFunctions: [...state.tables.trackedFunctions],
  serverVersion: state.main.serverVersion ? state.main.serverVersion : '',
});

const schemaConnector = connect => connect(mapStateToProps)(Schema);

export default schemaConnector;
