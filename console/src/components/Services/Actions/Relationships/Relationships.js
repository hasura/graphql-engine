import React from 'react';
import TypeRelationship from './Components/TypeRelationship';

const Relationships = ({ objectType, allTables, schemaList, dispatch }) => {
  const existingRelationships = (objectType.relationships || []).map(r => {
    return (
      <TypeRelationship
        dispatch={dispatch}
        relConfig={r}
        objectType={objectType}
        typename={objectType.name}
        allTables={allTables}
        schemaList={schemaList}
      />
    );
  });

  return (
    <div>
      {existingRelationships}
      <TypeRelationship
        dispatch={dispatch}
        objectType={objectType}
        typename={objectType.name}
        allTables={allTables}
        schemaList={schemaList}
      />
    </div>
  );
};

export default Relationships;
