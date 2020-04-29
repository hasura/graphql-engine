import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';
import { Link as RouterLink } from 'react-router';

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
import GqlCompatibilityWarning from '../../../Common/GqlCompatibilityWarning/GqlCompatibilityWarning';
import {
  displayTableName,
  getFunctionName,
  getSchemaTables,
  getUntrackedTables,
} from '../../../Common/utils/pgUtils';
import { isEmpty } from '../../../Common/utils/jsUtils';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import RawSqlButton from '../Common/Components/RawSqlButton';
import { Icon, ToolTip, Heading, Link, Box, Flex } from '../../../UIKit/atoms';
import styles from '../../../Common/Common.scss';

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
      readOnlyMode,
      untrackedRelations,
      currentSchema,
      dispatch,
      functionsList,
      nonTrackableFunctions,
      trackedFunctions,
    } = this.props;

    const handleSchemaChange = e => {
      dispatch(updateCurrentSchema(e.target.value));
    };

    /***********/

    const _getTrackableFunctions = () => {
      const trackedFuncNames = trackedFunctions.map(fn => getFunctionName(fn));

      // Assuming schema for both function and tables are same
      // return function which are tracked
      const filterCondition = func => {
        return !trackedFuncNames.includes(getFunctionName(func));
      };

      return functionsList.filter(filterCondition);
    };

    const getSectionHeading = (headingText, tooltip, actionElement = null) => {
      return (
        <div>
          <Heading type="subHeading" display="inline-block">
            {headingText}
          </Heading>
          <ToolTip message={tooltip} ml="sm" mr="20px" />
          {actionElement}
        </div>
      );
    };

    /***********/

    const allUntrackedTables = getUntrackedTables(
      getSchemaTables(schema, currentSchema)
    );
    const trackableFuncs = _getTrackableFunctions();

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
              <Icon type="add" size={10} />
            </Button>
          );

          const openCreateSection = (
            <Box display="inline-block" ml="20px">
              <Box display="inline-block">
                <input
                  id="schema-name-input"
                  type="text"
                  value={schemaNameEdit}
                  onChange={handleSchemaNameChange}
                  placeholder="schema_name"
                  className={'form-control input-sm ' + styles.display_inline}
                />
              </Box>
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
            </Box>
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
              <Icon type="delete" size={10} />
            </Button>
          );
        }

        return deleteSchemaBtn;
      };

      return (
        <Box mt="20px">
          <Box display="inline-block">Current Postgres schema</Box>
          <Box display="inline-block">
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
          </Box>
          <Flex display="inline-block" ml="20px">
            {getDeleteSchemaBtn()}
            <Box ml="10px" display="inline-block">
              {getCreateSchemaSection()}
            </Box>
          </Flex>
        </Box>
      );
    };

    const getUntrackedTablesSection = () => {
      const getTrackAllBtn = () => {
        if (readOnlyMode) {
          return null;
        }

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
              className={`${styles.display_inline}`}
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

        if (isEmpty(allUntrackedTables)) {
          untrackedTablesList.push(
            <div key="no-untracked">There are no untracked tables or views</div>
          );
        } else {
          allUntrackedTables.forEach((table, i) => {
            const tableName = table.table_name;

            const getTrackBtn = () => {
              if (readOnlyMode) {
                return null;
              }

              const handleTrackTable = e => {
                e.preventDefault();

                dispatch(setTableName(tableName));
                dispatch(addExistingTableSql());
              };

              return (
                <div
                  className={`${styles.display_inline} ${styles.add_mar_right}`}
                >
                  <Button
                    data-test={`add-track-table-${tableName}`}
                    className={`${styles.display_inline}`}
                    color="white"
                    size="xs"
                    onClick={handleTrackTable}
                  >
                    Track
                  </Button>
                </div>
              );
            };

            untrackedTablesList.push(
              <div className={styles.padd_bottom} key={`untracked-${i}`}>
                {getTrackBtn()}
                <div className={styles.display_inline}>
                  {displayTableName(table)}
                </div>
                <GqlCompatibilityWarning identifier={tableName} />
              </div>
            );
          });
        }

        return untrackedTablesList;
      };

      const heading = getSectionHeading(
        'Untracked tables or views',
        'Tables or views that are not exposed over the GraphQL API',
        getTrackAllBtn()
      );

      return (
        <Box mt="20px">
          <CollapsibleToggle title={heading} isOpen>
            <div className={`${styles.padd_left_remove} col-xs-12`}>
              {getUntrackedTablesList()}
            </div>
            <div className={styles.clear_fix} />
          </CollapsibleToggle>
        </Box>
      );
    };

    const getUntrackedRelationsSection = () => {
      const getTrackAllBtn = () => {
        if (readOnlyMode) {
          return null;
        }

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
              className={`${styles.display_inline}`}
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

        if (isEmpty(untrackedRelations)) {
          untrackedRelList.push(
            <div key="no-untracked-rel">There are no untracked relations</div>
          );
        } else {
          untrackedRelations.forEach((rel, i) => {
            const relData = rel.data;

            const getTrackBtn = () => {
              if (readOnlyMode) {
                return null;
              }

              const handleTrackRel = e => {
                e.preventDefault();

                dispatch(autoAddRelName(rel));
              };

              return (
                <div
                  className={`${styles.display_inline} ${styles.add_mar_right}`}
                >
                  <Button
                    className={styles.display_inline}
                    color="white"
                    size="xs"
                    onClick={handleTrackRel}
                  >
                    Track
                  </Button>
                </div>
              );
            };

            const relFrom = <b>{relData.lTable}</b>;

            const relTo = relData.isObjRel ? (
              <b>{relData.rTable}</b>
            ) : (
              <b>[ {relData.rTable} ]</b>
            );

            untrackedRelList.push(
              <div className={styles.padd_bottom} key={`untracked-rel-${i}`}>
                {getTrackBtn()}
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
        }

        return untrackedRelList;
      };

      const heading = getSectionHeading(
        'Untracked foreign-key relations',
        'Relationships inferred via foreign-keys that are not exposed over the GraphQL API',
        getTrackAllBtn()
      );

      return (
        <Box mt="20px">
          <CollapsibleToggle title={heading} isOpen>
            <div className={`${styles.padd_left_remove} col-xs-12`}>
              {getUntrackedRelList()}
            </div>
            <div className={styles.clear_fix} />
          </CollapsibleToggle>
        </Box>
      );
    };

    const getUntrackedFunctionsSection = () => {
      const noTrackableFunctions = isEmpty(trackableFuncs);

      const getTrackableFunctionsList = () => {
        const trackableFunctionList = [];

        if (noTrackableFunctions) {
          trackableFunctionList.push(
            <div key="no-untracked-fns">
              <div>There are no untracked functions</div>
            </div>
          );
        } else {
          trackableFuncs.forEach((p, i) => {
            const getTrackBtn = () => {
              if (readOnlyMode) {
                return null;
              }

              const handleTrackFn = e => {
                e.preventDefault();

                dispatch(addExistingFunction(p.function_name));
              };

              return (
                <div className={styles.display_inline}>
                  <Button
                    data-test={`add-track-function-${p.function_name}`}
                    className={`${styles.display_inline} btn btn-xs btn-default`}
                    onClick={handleTrackFn}
                  >
                    Track
                  </Button>
                </div>
              );
            };

            trackableFunctionList.push(
              <div
                className={styles.padd_bottom}
                key={`untracked-function-${i}`}
              >
                {getTrackBtn()}
                <div
                  className={`${styles.display_inline} ${styles.add_mar_left_mid}`}
                >
                  <RawSqlButton
                    dataTestId={`view-function-${p.function_name}`}
                    customStyles={styles.display_inline}
                    sql={p.function_definition}
                    dispatch={dispatch}
                  >
                    View
                  </RawSqlButton>
                </div>
                <div
                  className={`${styles.display_inline} ${styles.add_mar_left}`}
                >
                  <span>{p.function_name}</span>
                </div>
              </div>
            );
          });
        }

        return trackableFunctionList;
      };

      const heading = getSectionHeading(
        'Untracked custom functions',
        'Custom functions that are not exposed over the GraphQL API',
        <Link
          type="moreInfo"
          ml="xs"
          href="https://hasura.io/docs/1.0/graphql/manual/queries/custom-functions.html"
        />
      );

      return (
        <Box mt="20px" key={'custom-functions-content'}>
          <CollapsibleToggle
            title={heading}
            isOpen={!noTrackableFunctions}
            testId={'toggle-trackable-functions'}
          >
            <Box pl="0px" className="col-xs-12">
              {getTrackableFunctionsList()}
            </Box>
            <div className={styles.clear_fix} />
          </CollapsibleToggle>
        </Box>
      );
    };

    const getNonTrackableFunctionsSection = () => {
      const noNonTrackableFuncs = isEmpty(nonTrackableFunctions);

      const getNonTrackableFuncList = () => {
        const nonTrackableFunctionList = [];

        if (noNonTrackableFuncs) {
          nonTrackableFunctionList.push(
            <div key="no-nontracked-fns">
              <div>There are no non trackable functions</div>
            </div>
          );
        } else {
          nonTrackableFunctions.forEach((p, i) => {
            nonTrackableFunctionList.push(
              <div
                className={styles.padd_bottom}
                key={`untracked-function-${i}`}
              >
                <div
                  className={`${styles.display_inline} ${styles.add_mar_right}`}
                >
                  <RawSqlButton
                    dataTestId={`view-function-${p.function_name}`}
                    customStyles={styles.display_inline}
                    sql={p.function_definition}
                    dispatch={dispatch}
                  >
                    View
                  </RawSqlButton>
                </div>
                <div className={styles.display_inline}>{p.function_name}</div>
              </div>
            );
          });
        }
        return nonTrackableFunctionList;
      };

      const heading = getSectionHeading(
        'Non trackable functions',
        'Functions that do not conform to Hasura requirements to be exposed over the GraphQL API'
      );

      return (
        <Box key={'non-trackable-custom-functions'} mt="20px">
          <CollapsibleToggle title={heading} isOpen={false}>
            <div className={`${styles.padd_left_remove} col-xs-12`}>
              {getNonTrackableFuncList()}
            </div>
            <div className={styles.clear_fix} />
          </CollapsibleToggle>
        </Box>
      );
    };

    const getPermissionsSummaryLink = () => {
      return (
        <Box mt="20px">
          <RouterLink to={getSchemaPermissionsRoute(currentSchema)}>
            Schema permissions summary
          </RouterLink>
        </Box>
      );
    };

    return (
      <Box pl="15px" pt="20px" className="container-fluid">
        <Helmet title="Schema - Data | Hasura" />
        <Flex>
          <Heading as="h2" display="inline-block" fontSize="h2">
            Schema
          </Heading>
          {getCreateBtn()}
        </Flex>
        {getCurrentSchemaSection()}
        {getUntrackedTablesSection()}
        {getUntrackedRelationsSection()}
        {getUntrackedFunctionsSection()}
        {getNonTrackableFunctionsSection()}
        <hr />
        {getPermissionsSummaryLink()}
      </Box>
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
  readOnlyMode: state.main.readOnlyMode,
  untrackedRelations: state.tables.untrackedRelations,
  currentSchema: state.tables.currentSchema,
  functionsList: [...state.tables.postgresFunctions],
  nonTrackableFunctions: [...state.tables.nonTrackablePostgresFunctions],
  trackedFunctions: [...state.tables.trackedFunctions],
  serverVersion: state.main.serverVersion ? state.main.serverVersion : '',
});

const schemaConnector = connect => connect(mapStateToProps)(Schema);

export default schemaConnector;
