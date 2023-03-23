import React, { useEffect } from 'react';
import PropTypes from 'prop-types';
import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';
import {
  FeatureFlagToast,
  useFeatureFlags,
  availableFeatureFlagIds,
} from '../../../../features/FeatureFlags';
import { DatabaseRelationshipsTab } from '../../../../features/DatabaseRelationships';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';
import { areTablesEqual } from '../../../../features/hasura-metadata-api';
import TableHeader from '../TableCommon/TableHeader';
import { getObjArrRelList } from './utils';
import { setTable, UPDATE_REMOTE_SCHEMA_MANUAL_REL } from '../DataActions';
import AddManualRelationship from './AddManualRelationship';
import RelationshipEditor from './RelationshipEditor';
import { NotFoundError } from '../../../Error/PageNotFound';
import RemoteRelationships from './RemoteRelationships/RemoteRelationships';
import ToolTip from '../../../Common/Tooltip/Tooltip';
import {
  currentDriver,
  findAllFromRel,
  isFeatureSupported,
} from '../../../../dataSources';
import { getRemoteSchemasSelector } from '../../../../metadata/selector';
import { RightContainer } from '../../../Common/Layout/RightContainer';
import { resetRelationshipForm, resetManualRelationshipForm } from './Actions';
import { RemoteDbRelationships } from './RemoteDbRelationships/RemoteDbRelationships';
import styles from '../TableModify/ModifyTable.module.scss';
import tableStyles from '../../../Common/TableCommon/TableStyles.module.scss';

const RelationshipsView = ({
  tableName,
  allSchemas,
  ongoingRequest,
  lastError,
  lastFormError,
  lastSuccess,
  dispatch,
  allFunctions,
  manualRelAdd,
  currentSchema,
  migrationMode,
  readOnlyMode,
  schemaList,
  remoteSchemas,
  currentSource,
  source,
}) => {
  useEffect(() => {
    dispatch(resetRelationshipForm());
    dispatch(resetManualRelationshipForm());
    dispatch(setTable(tableName));
    // Sourcing the current schema into manual relationship
    dispatch({
      type: UPDATE_REMOTE_SCHEMA_MANUAL_REL,
      data: currentSchema,
    });
  }, [tableName, currentSchema, dispatch]);

  const tableSchema = allSchemas.find(
    t => t.table_name === tableName && t.table_schema === currentSchema
  );

  const normalizedTableObject = source.tables.find(t => {
    return areTablesEqual(t.table, {
      table_schema: tableSchema.table_schema,
      table_name: tableSchema.table_name,
    });
  });

  const { data: featureFlagsData, isLoading: isFeatureFlagsLoading } =
    useFeatureFlags();
  if (isFeatureFlagsLoading) return <div>Loading...</div>;

  const newRelationshipsTabIsEnabled =
    featureFlagsData &&
    featureFlagsData?.length > 0 &&
    featureFlagsData.find(
      featureFlag =>
        featureFlag.id === availableFeatureFlagIds.relationshipTabTablesId
    )?.state?.enabled;

  if (newRelationshipsTabIsEnabled) {
    return (
      <DatabaseRelationshipsTab
        table={tableSchema}
        metadataTable={normalizedTableObject.table}
        driver={currentDriver}
        currentSource={currentSource}
        migrationMode={migrationMode}
      />
    );
  }

  if (!tableSchema) {
    // throw a 404 exception
    throw new NotFoundError();
  }

  let alert = null;
  if (ongoingRequest) {
    alert = (
      <div className="hidden alert alert-warning" role="alert">
        Saving...
      </div>
    );
  } else if (lastError) {
    alert = (
      <div className="hidden alert alert-danger" role="alert">
        Error: {JSON.stringify(lastError)}
      </div>
    );
  } else if (lastSuccess) {
    alert = (
      <div className="hidden alert alert-success" role="alert">
        Saved!
      </div>
    );
  } else if (lastFormError) {
    alert = (
      <div className="hidden alert alert-warning" role="alert">
        {lastFormError}
      </div>
    );
  }

  const objArrRelList = getObjArrRelList(tableSchema.relationships);

  let addedRelationshipsView = null;
  if (objArrRelList.length > 0) {
    addedRelationshipsView = (
      <div className={tableStyles.tableContainer}>
        <table
          className={`${tableStyles.table} table table-bordered table-striped table-hover`}
        >
          <thead>
            <tr>
              {['Object relationships', 'Array relationships'].map((s, i) => (
                <th key={i}>{s}</th>
              ))}
            </tr>
          </thead>
          <tbody>
            {objArrRelList.map(rel => {
              const column1 = rel.objRel ? (
                <RelationshipEditor
                  dispatch={dispatch}
                  key={rel.objRel.rel_name}
                  relConfig={findAllFromRel(tableSchema, rel.objRel)}
                />
              ) : (
                <td />
              );
              const column2 = rel.arrRel ? (
                <RelationshipEditor
                  key={rel.arrRel.rel_name}
                  dispatch={dispatch}
                  relConfig={findAllFromRel(tableSchema, rel.arrRel)}
                />
              ) : (
                <td />
              );
              return (
                <tr>
                  {column1}
                  {column2}
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>
    );
  }

  const remoteRelationshipsSection = () => {
    const existingRemoteRelationships =
      tableSchema?.remote_relationships?.filter(
        field =>
          'remote_schema' in field.definition ||
          'to_remote_schema' in field.definition
      ) ?? [];

    return (
      <div className={`${styles.padd_left_remove} col-xs-10 col-md-10`}>
        <RemoteRelationships
          relationships={existingRemoteRelationships}
          reduxDispatch={dispatch}
          table={tableSchema}
          allFunctions={allFunctions}
          remoteSchemas={remoteSchemas}
        />
      </div>
    );
  };

  return (
    <>
      <RightContainer>
        <Analytics name="RelationshipsView" {...REDACT_EVERYTHING}>
          <div className={`${styles.container} container-fluid`}>
            <TableHeader
              dispatch={dispatch}
              table={tableSchema}
              source={currentSource}
              tabName="relationships"
              migrationMode={migrationMode}
              readOnlyMode={readOnlyMode}
            />
            <br />
            <div className={`${styles.padd_left_remove} container-fluid`}>
              <div
                className={`${styles.padd_left_remove} ${styles.add_mar_bottom} col-xs-10 col-md-10`}
              >
                <h4 className={styles.subheading_text}>
                  Table Relationships
                  <ToolTip message={'Relationships to tables / views'} />
                  &nbsp;
                  <LearnMoreLink href="https://hasura.io/docs/latest/graphql/core/schema/table-relationships/index.html" />
                </h4>
                {addedRelationshipsView}
                <div className={styles.activeEdit}>
                  <AddManualRelationship
                    tableSchema={tableSchema}
                    allSchemas={allSchemas}
                    schemaList={schemaList}
                    relAdd={manualRelAdd}
                    dispatch={dispatch}
                  />
                </div>
              </div>
              {isFeatureSupported(
                'tables.relationships.remoteDbRelationships.hostSource'
              ) ? (
                <div
                  className={`${styles.padd_left_remove} col-xs-10 col-md-10 ${styles.add_mar_bottom}`}
                >
                  <RemoteDbRelationships
                    tableSchema={tableSchema}
                    reduxDispatch={dispatch}
                    currentSource={currentSource}
                  />
                </div>
              ) : null}
              {isFeatureSupported('tables.relationships.track') &&
                remoteRelationshipsSection()}
            </div>
            <div className={`${styles.fixed} hidden`}>{alert}</div>
          </div>
        </Analytics>
      </RightContainer>
      <FeatureFlagToast
        flagId={availableFeatureFlagIds.relationshipTabTablesId}
      />
    </>
  );
};

RelationshipsView.propTypes = {
  tableName: PropTypes.string.isRequired,
  allSchemas: PropTypes.array.isRequired,
  currentSchema: PropTypes.string.isRequired,
  activeEdit: PropTypes.object.isRequired,
  manualRelAdd: PropTypes.object.isRequired,
  ongoingRequest: PropTypes.bool.isRequired,
  migrationMode: PropTypes.bool.isRequired,
  readOnlyMode: PropTypes.bool.isRequired,
  lastError: PropTypes.object,
  lastFormError: PropTypes.object,
  lastSuccess: PropTypes.bool,
  dispatch: PropTypes.func.isRequired,
  serverVersion: PropTypes.string,
  allFunctions: PropTypes.array.isRequired,
  remoteSchemas: PropTypes.array.isRequired,
  featuresCompatibility: PropTypes.object,
};

const mapStateToProps = (state, ownProps) => {
  const {
    nonTrackablePostgresFunctions: nonTrackableFns,
    postgresFunctions: trackedFns,
  } = state.tables;
  return {
    tableName: ownProps.params.table,
    allSchemas: state.tables.allSchemas,
    currentSchema: state.tables.currentSchema,
    migrationMode: state.main.migrationMode,
    readOnlyMode: state.main.readOnlyMode,
    serverVersion: state.main.serverVersion,
    allFunctions: nonTrackableFns?.concat(trackedFns ?? []) ?? [],
    schemaList: state.tables.schemaList,
    remoteSchemas: getRemoteSchemasSelector(state).map(schema => schema.name),
    currentSource: state.tables.currentDataSource,
    source: state.metadata.metadataObject.sources.find(
      s => s.name === state.tables.currentDataSource
    ),
    ...state.tables.modify,
  };
};

const relationshipsViewConnector = connect =>
  connect(mapStateToProps)(RelationshipsView);

export default relationshipsViewConnector;
