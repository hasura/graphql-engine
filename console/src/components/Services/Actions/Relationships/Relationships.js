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
}) => {
  const existingRelationships = (objectType.relationships || []).map(r => {
    return (
      <TypeRelationship
        dispatch={dispatch}
        relConfig={r}
        objectType={objectType}
        typename={objectType.name}
        allTables={allTables}
        schemaList={schemaList}
        readOnlyMode={readOnlyMode}
      />
    );
  });

  return (
    <div>
      <Helmet
        title={`Relationships - ${currentAction.action_name} - Actions | Hasura`}
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
      />
    </div>
  );
};

export default Relationships;
