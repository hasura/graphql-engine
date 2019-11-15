import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';
import { Link } from 'react-router';

import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';

import {
  untrackedTablesTip,
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
import { getRelDef } from '../TableRelationships/utils';
import {
  getSchemaAddTableRoute,
  getSchemaPermissionsRoute,
} from '../../../Common/utils/routesUtils';
import { createNewSchema, deleteCurrentSchema } from './Actions';
import CollapsibleToggle from '../../../Common/CollapsibleToggle/CollapsibleToggle';
import gqlPattern from '../Common/GraphQLValidation';
import GqlCompatibilityWarning from '../../../Common/GqlCompatibilityWarning/GqlCompatibilityWarning';
import {
  displayTableName,
  getFunctionName,
  getSchemaTables,
  getUntrackedTables,
} from '../../../Common/utils/pgUtils';
import { SET_SQL } from '../RawSQL/Actions';
import _push from '../push';
import { isEmpty } from '../../../Common/utils/jsUtils';
import { getConfirmation } from '../../../Common/utils/jsUtils';

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

    const _getTrackableFunctions = () => {
      const trackedFuncNames = trackedFunctions.map(fn => getFunctionName(fn));

      // Assuming schema for both function and tables are same
      // return function which are tracked && function name whose
      // set of tables are tracked
      const filterCondition = func => {
        return (
          !trackedFuncNames.includes(getFunctionName(func)) &&
          !!func.return_table_info
        );
      };

      return functionsList.filter(filterCondition);
    };

    const getSectionHeading = (headingText, tooltip, actionBtn = null) => {
      return (
        <div>
          <h4
            className={`${styles.subheading_text} ${styles.display_inline} ${
              styles.add_mar_right_mid
            }`}
          >
            {headingText}
          </h4>
          <OverlayTrigger placement="right" overlay={tooltip}>
            <i className="fa fa-info-circle" aria-hidden="true" />
          </OverlayTrigger>
          {actionBtn}
        </div>
      );
    };

    /***********/

    const allUntrackedTables = getUntrackedTables(
      getSchemaTables(schema, currentSchema)
    );
    const trackableFuncs = _getTrackableFunctions();

    const getTrackableFunctionsRequirementsMessage = () => {
      const requirementsLink = (
        <a
          href="https://docs.hasura.io/1.0/graphql/manual/queries/custom-functions.html#supported-sql-functions"
          target="_blank"
          rel="noopener noreferrer"
        >
          requirements
        </a>
      );

      return (
        <i>
          See {requirementsLink} for functions to be exposed over the GraphQL
          API
        </i>
      );
    };

    const getCreateBtn = () => {
      let createBtn = null;

      if (migrationMode) {
        const handleClick = e => {
          e.preventDefault();

          dispatch(push(getSchemaAddTableRoute(currentSchema)));
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

            if (!schemaName) {
              document.getElementById('schema-name-input').focus();
              return;
            }

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
                  id="schema-name-input"
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

          const isOk = getConfirmation(
            'This will expose all the listed tables/views over the GraphQL API'
          );
          if (!isOk) {
            return;
          }

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

          const isGQLCompatible = gqlPattern.test(table.table_name);
          const gqlCompatibilityWarning = !isGQLCompatible ? (
            <span className={styles.add_mar_left_mid}>
              <GqlCompatibilityWarning />
            </span>
          ) : null;

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
              <div className={styles.display_inline}>
                {displayTableName(table)}
              </div>
              {gqlCompatibilityWarning}
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

      const heading = getSectionHeading(
        'Untracked tables or views',
        untrackedTablesTip,
        getTrackAllBtn()
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

          const isOk = getConfirmation(
            'This will add all the listed foreign-keys as relationships in the GraphQL schema'
          );
          if (!isOk) {
            return;
          }

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

      const heading = getSectionHeading(
        'Untracked foreign-key relations',
        untrackedRelTip,
        getTrackAllBtn()
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
      const noTrackableFunctions = isEmpty(trackableFuncs);

      const getTrackableFunctionsList = () => {
        const trackableFunctionList = [];

        trackableFuncs.forEach((p, i) => {
          trackableFunctionList.push(
            <div className={styles.padd_bottom} key={`untracked-function-${i}`}>
              <div
                className={`${styles.display_inline} ${styles.add_mar_right}`}
              >
                <Button
                  data-test={`add-track-function-${p.function_name}`}
                  className={`${styles.display_inline} btn btn-xs btn-default`}
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
          );
        });

        if (noTrackableFunctions) {
          trackableFunctionList.push(
            <div key="no-untracked-fns">
              <div>There are no untracked functions</div>
              <div className={styles.add_mar_top}>
                {getTrackableFunctionsRequirementsMessage()}
              </div>
            </div>
          );
        }

        return trackableFunctionList;
      };

      const heading = getSectionHeading(
        'Untracked custom functions',
        trackableFunctionsTip
      );

      return (
        <div className={styles.add_mar_top} key={'custom-functions-content'}>
          <CollapsibleToggle
            title={heading}
            isOpen={!noTrackableFunctions}
            testId={'toggle-trackable-functions'}
          >
            <div className={`${styles.padd_left_remove} col-xs-12`}>
              {getTrackableFunctionsList()}
            </div>
            <div className={styles.clear_fix} />
          </CollapsibleToggle>
        </div>
      );
    };

    const getNonTrackableFunctionsSection = () => {
      let nonTrackableFuncList = null;

      if (nonTrackableFunctions.length > 0) {
        const heading = getSectionHeading(
          'Non trackable functions',
          nonTrackableFunctionsTip
        );

        nonTrackableFuncList = (
          <div
            className={styles.add_mar_top}
            key={'non-trackable-custom-functions'}
          >
            <CollapsibleToggle title={heading} isOpen={false}>
              <div className={`${styles.padd_left_remove} col-xs-12`}>
                <div className={styles.add_mar_bottom}>
                  {getTrackableFunctionsRequirementsMessage()}
                </div>
                {nonTrackableFunctions.map((p, i) => (
                  <div
                    className={styles.padd_bottom}
                    key={`untracked-function-${i}`}
                  >
                    <div
                      className={`${styles.display_inline} ${
                        styles.add_mar_right
                      }`}
                    >
                      <Button
                        data-test={`view-function-${p.function_name}`}
                        className={`${
                          styles.display_inline
                        } btn btn-xs btn-default`}
                        onClick={e => {
                          e.preventDefault();

                          dispatch(_push('/data/sql'));

                          dispatch({
                            type: SET_SQL,
                            data: p.function_definition,
                          });
                        }}
                      >
                        View
                      </Button>
                    </div>
                    <div className={styles.display_inline}>
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

    const getPermissionsSummaryLink = () => {
      return (
        <div className={styles.add_mar_top}>
          <Link to={getSchemaPermissionsRoute(currentSchema)}>
            Schema permissions summary
          </Link>
        </div>
      );
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
          {getNonTrackableFunctionsSection()}
          <hr />
          {getPermissionsSummaryLink()}
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
