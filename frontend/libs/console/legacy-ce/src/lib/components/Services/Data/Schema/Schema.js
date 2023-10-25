/* eslint-disable no-unused-vars */
import PropTypes from 'prop-types';
import React, { Component } from 'react';
import Helmet from 'react-helmet';
import { Link } from 'react-router';

import { PrimaryDBInfo } from '../../../Common/EditableHeading/PrimaryDBInfo';
import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';
import { CustomFieldNames } from '../../../../features/Data';
import { Button } from '../../../../new-components/Button';
import { TableTrackingCustomizationModalKey } from '../../../../store/modal/modal.constants';
import { BsBoxArrowUpRight } from 'react-icons/bs';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';
import { FaCog, FaDatabase, FaFolder, FaPlusCircle } from 'react-icons/fa';
import {
  currentDriver,
  dataSource,
  getSchemaTables,
  getUntrackedTables,
  isFeatureSupported,
} from '../../../../dataSources';
import { getConsistentFunctions } from '../../../../metadata/selector';
import { hideModal, showModal } from '../../../../store/modal/modal.actions';
import { modalKeySelector } from '../../../../store/modal/modal.selectors';
import CollapsibleToggle from '../../../Common/CollapsibleToggle/CollapsibleToggle';
import styles from '../../../Common/Common.module.scss';
import GqlCompatibilityWarning from '../../../Common/GqlCompatibilityWarning/GqlCompatibilityWarning';

import BreadCrumb from '../../../Common/Layout/BreadCrumb/BreadCrumb';
import { RightContainer } from '../../../Common/Layout/RightContainer';
import ToolTip from '../../../Common/Tooltip/Tooltip';
import { getConfirmation, isEmpty } from '../../../Common/utils/jsUtils';
import {
  getDataSourceBaseRoute,
  getSchemaAddTableRoute,
  getSchemaBaseRoute,
  getSchemaPermissionsRoute,
} from '../../../Common/utils/routesUtils';
import {
  addAllUntrackedTablesSql,
  addExistingTableSql,
  setTableName,
} from '../Add/AddExistingTableViewActions';
import RawSqlButton from '../Common/Components/RawSqlButton';
import {
  fetchFunctionInit,
  updateCurrentSchema,
  updateSchemaInfo,
} from '../DataActions';
import _push from '../push';
import {
  autoAddRelName,
  autoTrackRelations,
} from '../TableRelationships/Actions';
import { getRelDef } from '../TableRelationships/utils';
import { createNewSchema, deleteCurrentSchema } from './Actions';
import { EmptyState } from './components/EmptyState/EmptyState';
import { TrackableFunctionsList } from './FunctionsList';
import { getTrackableFunctions } from './utils';
import { FeatureFlagContainer } from './TrackTablesContainer';

const DeleteSchemaButton = ({ dispatch, migrationMode, currentDataSource }) => {
  const successCb = () => {
    dispatch(updateCurrentSchema('public', currentDataSource));
  };

  const handleDelete = () => {
    dispatch(deleteCurrentSchema(successCb));
  };

  return (
    migrationMode && (
      <Button
        size="sm"
        title="Delete current schema"
        onClick={handleDelete}
        style={{ marginRight: '20px' }}
      >
        Delete Schema
      </Button>
    )
  );
};

const OpenCreateSection = React.forwardRef(
  ({ ref, value, handleInputChange, handleCreate, handleCancelCreate }) => (
    <div className={styles.display_inline}>
      <div className={styles.display_inline}>
        <input
          type="text"
          value={value}
          onChange={handleInputChange}
          placeholder="schema_name"
          className={`form-control input-sm ${styles.display_inline}`}
          ref={ref}
        />
      </div>
      <Button
        size="sm"
        onClick={handleCreate}
        className={styles.add_mar_left_mid}
      >
        Create New Schema
      </Button>
      <Button
        size="sm"
        onClick={handleCancelCreate}
        className={styles.add_mar_left_mid}
      >
        Cancel
      </Button>
    </div>
  )
);

const ClosedCreateSection = ({ onClick }) => (
  <Button size="sm" onClick={onClick} title="Create new schema">
    Create New Schema
  </Button>
);

const CreateSchemaSection = React.forwardRef(
  ({
    ref,
    schema,
    migrationMode,
    createSchemaOpen,
    schemaNameEdit,
    handleCancelCreateNewSchema,
    handleCreateNewClick,
    handleSchemaNameChange,
    handleCreateClick,
    currentDataSource,
  }) =>
    migrationMode && (
      <div className={`${styles.display_flex}`}>
        {isFeatureSupported('schemas.create.enabled') ? (
          <span>
            {createSchemaOpen ? (
              <OpenCreateSection
                ref={ref}
                value={schemaNameEdit}
                handleInputChange={handleSchemaNameChange}
                handleCreate={handleCreateClick}
                handleCancelCreate={handleCancelCreateNewSchema}
              />
            ) : (
              <ClosedCreateSection onClick={handleCreateNewClick} />
            )}
          </span>
        ) : null}
        <SchemaPermissionsButton schema={schema} source={currentDataSource} />
      </div>
    )
);

const SchemaPermissionsButton = ({ schema, source }) => (
  <Link
    to={getSchemaPermissionsRoute(schema, source)}
    style={{ marginLeft: '20px' }}
  >
    <Button size="sm">Show Permissions Summary</Button>
  </Link>
);

class Schema extends Component {
  constructor(props) {
    super(props);

    this.state = {
      isExporting: false,
      createSchemaOpen: false,
      schemaNameEdit: '',
      loadingSchemas: false,
      isTrackTableCustomizing: false,
    };

    this.props.dispatch(fetchFunctionInit());
    this.props.dispatch(updateSchemaInfo({ schemas: this.props.schemaList }));

    this.schemaNameInputRef = React.createRef(null);
  }

  componentDidUpdate(prevProps) {
    if (prevProps.currentSchema !== this.props.currentSchema) {
      this.props.dispatch(updateSchemaInfo({ schemas: this.props.schemaList }));
    }
  }

  cancelCreateNewSchema = () => {
    this.setState({
      createSchemaOpen: false,
    });
  };

  onCreateNewClick = () => {
    this.setState({ createSchemaOpen: true });
  };

  setCustomizedTableName = tableName => {
    this.setState({ customizedTableName: tableName });
  };

  onChangeSchemaName = e => {
    this.setState({ schemaNameEdit: e.target.value });
  };

  handleCreateClick = () => {
    const schemaName = this.state.schemaNameEdit.trim();

    if (!schemaName) {
      this.schemaNameInputRef.current.focus();
      return;
    }

    const successCb = () => {
      this.props.dispatch(
        updateCurrentSchema(schemaName, this.props.currentDataSource)
      );

      this.setState({
        schemaNameEdit: '',
        createSchemaOpen: false,
      });
    };

    this.props.dispatch(createNewSchema(schemaName, successCb));
  };

  render() {
    const {
      schema,
      migrationMode,
      readOnlyMode,
      untrackedRelations,
      currentSchema,
      dispatch,
      functionsList,
      nonTrackableFunctions,
      trackedFunctions,
      currentDataSource,
      modalKey,
      showTableTrackingModal,
      hideTableTrackingModal,
    } = this.props;

    const getSectionHeading = (headingText, tooltip, actionElement = null) => {
      return (
        <div>
          <h4 className={`${styles.subheading_text} ${styles.display_inline}`}>
            {headingText}
          </h4>
          <span className={styles.add_mar_left_small}>
            <ToolTip message={tooltip} />
          </span>
          {actionElement}
        </div>
      );
    };

    const allUntrackedTables = getUntrackedTables(
      getSchemaTables(schema, currentSchema)
    );

    const trackableFuncs = getTrackableFunctions(
      functionsList,
      trackedFunctions
    );
    const getCreateBtn = (size = 'md') => {
      let createBtn = null;

      if (migrationMode && isFeatureSupported('tables.create.enabled')) {
        const handleClick = e => {
          e.preventDefault();

          dispatch(
            _push(getSchemaAddTableRoute(currentSchema, currentDataSource))
          );
        };

        createBtn = (
          <Analytics
            name="data-tab-create-table-button"
            passHtmlAttributesToChildren
          >
            <Button
              mode="primary"
              size={size}
              className="p-0 m-0 ml-sm"
              data-test="data-create-table"
              onClick={handleClick}
              iconPosition="start"
              icon={<FaPlusCircle className="h-3" />}
            >
              Create Table
            </Button>
          </Analytics>
        );
      }

      return createBtn;
    };

    const getCurrentSchemaSection = () => {
      return (
        <div className={styles.add_mar_top}>
          <div style={{ marginTop: '20px' }}>
            <div className={styles.display_inline} style={{ width: '120px' }}>
              Database Schema
            </div>
            <div className={`${styles.display_inline} ${styles.add_mar_left}`}>
              <div style={{ display: 'flex', alignItems: 'center' }}>
                {isFeatureSupported('schemas.delete.enabled') ? (
                  <DeleteSchemaButton
                    dispatch={dispatch}
                    migrationMode={migrationMode}
                    currentDataSource={currentDataSource}
                    schemaList={this.props.schemaList}
                  />
                ) : null}
                <CreateSchemaSection
                  ref={this.schemaNameInputRef}
                  migrationMode={migrationMode}
                  schemaNameEdit={this.state.schemaNameEdit}
                  createSchemaOpen={this.state.createSchemaOpen}
                  handleCancelCreateNewSchema={this.cancelCreateNewSchema}
                  handleCreateNewClick={this.onCreateNewClick}
                  handleSchemaNameChange={this.onChangeSchemaName}
                  handleCreateClick={this.handleCreateClick}
                  currentDataSource={currentDataSource}
                  schema={currentSchema}
                />
              </div>
            </div>
          </div>
        </div>
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
            <Analytics
              name="data-tab-track-all-button"
              passHtmlAttributesToChildren
            >
              <Button
                size="sm"
                className={`${styles.display_inline}`}
                onClick={trackAllTables}
              >
                Track All
              </Button>
            </Analytics>
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

                this.setCustomizedTableName(tableName);
                dispatch(setTableName(tableName));
                dispatch(addExistingTableSql());
              };

              const onCustomizeTableButtonClick = () => {
                this.setCustomizedTableName(tableName);
                showTableTrackingModal();
              };

              return (
                <div
                  className={`${styles.display_inline} ${styles.add_mar_right}`}
                >
                  <Analytics
                    name="data-tab-track-table-button"
                    passHtmlAttributesToChildren
                  >
                    <Button
                      size="sm"
                      className={`${styles.display_inline}`}
                      data-test={`add-track-table-${tableName}`}
                      onClick={handleTrackTable}
                    >
                      Track
                    </Button>
                  </Analytics>

                  <Button
                    title="customize table before tracking"
                    size="sm"
                    icon={<FaCog />}
                    className={`ml-2 ${styles.display_inline}`}
                    onClick={onCustomizeTableButtonClick}
                  />
                </div>
              );
            };

            untrackedTablesList.push(
              <div className={styles.padd_bottom} key={`untracked-${i}`}>
                {getTrackBtn()}
                <div className={styles.display_inline}>
                  {dataSource.displayTableName(table)}
                </div>
                <GqlCompatibilityWarning
                  identifier={tableName}
                  className={styles.add_mar_left_mid}
                  ifWarningCanBeFixed
                />
              </div>
            );
          });
        }

        return untrackedTablesList;
      };

      const heading = getSectionHeading(
        'Untracked tables or views',
        'Tables or views that are not exposed over the GraphQL API',
        <span className={styles.add_mar_left}>{getTrackAllBtn()}</span>
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
              size="sm"
              className={`${styles.display_inline}`}
              data-test="track-all-relationships"
              onClick={trackAllRelations}
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
                    size="sm"
                    className={styles.display_inline}
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
        'Untracked foreign-key relationships',
        'Relationships inferred via foreign-keys that are not exposed over the GraphQL API',
        <>
          <LearnMoreLink href="https://hasura.io/docs/latest/graphql/core/schema/table-relationships/index.html" />
          <span className={styles.add_mar_left}>{getTrackAllBtn()}</span>
        </>
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

    const getUntrackedFunctionsSection = isSupported => {
      const heading = getSectionHeading(
        'Untracked custom functions',
        'Custom functions that are not exposed over the GraphQL API',
        <LearnMoreLink href="https://hasura.io/docs/latest/graphql/core/schema/custom-functions.html" />
      );

      return (
        <div className={styles.add_mar_top} key={'custom-functions-content'}>
          <CollapsibleToggle
            title={heading}
            isOpen
            testId={'toggle-trackable-functions'}
          >
            <div className={`${styles.padd_left_remove} col-xs-12`}>
              {isSupported ? (
                <TrackableFunctionsList
                  dispatch={dispatch}
                  funcs={trackableFuncs}
                  readOnlyMode={readOnlyMode}
                  source={currentDataSource}
                  allSchemas={schema}
                />
              ) : (
                `Currently unsupported for ${(
                  currentDriver + ''
                ).toUpperCase()}`
              )}
            </div>
            <div className={styles.clear_fix} />
          </CollapsibleToggle>
        </div>
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
                    source={currentDataSource}
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
        <div
          className={styles.add_mar_top}
          key={'non-trackable-custom-functions'}
        >
          <CollapsibleToggle title={heading}>
            <div className={`${styles.padd_left_remove} col-xs-12`}>
              {getNonTrackableFuncList()}
            </div>
            <div className={styles.clear_fix} />
          </CollapsibleToggle>
        </div>
      );
    };

    const getContent = () => {
      if (
        getSchemaTables(schema, currentSchema).length === 0 &&
        currentDriver !== 'bigquery'
      ) {
        return (
          <>
            <h2 className={`text-3xl font-bold display-inline`}>
              {currentSchema}
            </h2>
            <hr className="my-md" />
            <EmptyState
              title="It looks like you don't currently have any tables in your schema"
              subtitle="Need help getting started?"
              buttons={[
                <Button
                  onClick={() => {
                    window.open(
                      'https://hasura.io/docs/latest/getting-started/getting-started-cloud/#step-4-try-out-hasura'
                    );
                  }}
                  size="sm"
                  iconPosition="end"
                  icon={<BsBoxArrowUpRight className="h-3" />}
                >
                  Learn more about creating tables
                </Button>,
                getCreateBtn('sm'),
              ]}
            />
          </>
        );
      }
      return (
        <>
          <div className="flex">
            <h2 className={`text-3xl font-bold display-inline`}>
              {currentSchema}
            </h2>
            {getCreateBtn()}
          </div>
          <div className="pt-4">
            <PrimaryDBInfo source={currentDataSource} />
          </div>

          <hr className="my-md" />
          {getCurrentSchemaSection()}
          <hr className="my-md" />

          <FeatureFlagContainer
            dataSourceName={currentDataSource}
            schema={currentSchema}
            dispatch={dispatch}
          >
            {getUntrackedTablesSection()}
            {isFeatureSupported('tables.relationships.track') &&
              getUntrackedRelationsSection()}
            {getUntrackedFunctionsSection(
              isFeatureSupported('functions.track.enabled')
            )}
            {isFeatureSupported('functions.nonTrackableFunctions.enabled') &&
              getNonTrackableFunctionsSection()}
          </FeatureFlagContainer>

          <hr className="my-md" />
        </>
      );
    };

    return (
      <RightContainer>
        {modalKey === TableTrackingCustomizationModalKey && (
          <CustomFieldNames.LegacyModal
            onClose={() => hideTableTrackingModal()}
            tableName={this.state.customizedTableName}
            dataSource={currentDataSource}
            schema={currentSchema}
            driver={currentDriver}
            callToAction="Customize & Track"
            callToActionLoadingText="Saving..."
          />
        )}
        <Analytics name="Schema" {...REDACT_EVERYTHING}>
          <div
            className={`container-fluid ${styles.padd_left_remove} bootstrap-jail`}
          >
            <div className={styles.padd_left}>
              <Helmet title="Schema - Data | Hasura" />
              <BreadCrumb
                breadCrumbs={[
                  { url: `/data`, title: 'Data' },
                  {
                    url: getDataSourceBaseRoute(currentDataSource),
                    title: currentDataSource,
                    prefix: <FaDatabase />,
                  },
                  {
                    url: getSchemaBaseRoute(currentSchema, currentDataSource),
                    title: currentSchema,
                    prefix: <FaFolder />,
                  },
                ]}
              />
              {getContent()}
            </div>
          </div>
        </Analytics>
      </RightContainer>
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

const mapDispatchToProps = dispatch => ({
  showTableTrackingModal: () =>
    dispatch(showModal(TableTrackingCustomizationModalKey)),
  hideTableTrackingModal: () => dispatch(hideModal()),
  dispatch: dispatch,
});

const mapStateToProps = state => ({
  schema: state.tables.allSchemas,
  schemaList: state.tables.schemaList,
  migrationMode: state.main.migrationMode,
  readOnlyMode: state.main.readOnlyMode,
  untrackedRelations: state.tables.untrackedRelations,
  currentSchema: state.tables.currentSchema,
  functionsList: state.tables.postgresFunctions,
  nonTrackableFunctions: state.tables.nonTrackablePostgresFunctions,
  trackedFunctions: getConsistentFunctions(state),
  serverVersion: state.main.serverVersion ? state.main.serverVersion : '',
  metadata: state.metadata.metadataObject,
  currentDataSource: state.tables.currentDataSource,
  modalKey: modalKeySelector(state),
});

const schemaConnector = connect =>
  connect(mapStateToProps, mapDispatchToProps)(Schema);

export default schemaConnector;
