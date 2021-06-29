import React from 'react';
import Helmet from 'react-helmet';
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

  return (
    <div>
      <Helmet
        title={`Relationships - ${currentAction.name} - Actions | Hasura`}
      />
      {existingRelationships}
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
