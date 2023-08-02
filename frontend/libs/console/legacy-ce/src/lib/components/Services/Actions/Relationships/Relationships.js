import React from 'react';
import Helmet from 'react-helmet';
import { useGetAnalyticsAttributes } from '../../../../features/Analytics';
import TypeRelationship from './Components/TypeRelationship';

const Relationships = ({
  objectType,
  allTables,
  schemaList,
  dispatch,
  currentAction,
  readOnlyMode,
  dataSources,
}) => {
  const existingRelationships = (objectType.relationships || []).map(r => {
    return (
      <TypeRelationship
        key={r.name}
        dispatch={dispatch}
        relConfig={r}
        objectType={objectType}
        typename={objectType.name}
        allTables={allTables}
        schemaList={schemaList}
        readOnlyMode={readOnlyMode}
        dataSources={dataSources}
      />
    );
  });

  const titleAnalyticsAttributes = useGetAnalyticsAttributes(
    'ActionsRelationships',
    { redactText: true }
  );

  return (
    <div>
      <Helmet>
        <title
          {...titleAnalyticsAttributes}
        >{`Relationships - ${currentAction.name} - Actions | Hasura`}</title>
      </Helmet>
      {existingRelationships}
      {objectType.kind === 'scalar' ? (
        <div className="mb-sm text-gray-600 test-sm">
          Action relationships with scalar output types are not possible{' '}
        </div>
      ) : null}
      <TypeRelationship
        dispatch={dispatch}
        objectType={objectType}
        typename={objectType.name}
        allTables={allTables}
        schemaList={schemaList}
        isNew
        readOnlyMode={readOnlyMode}
        dataSources={dataSources}
      />
    </div>
  );
};

export default Relationships;
