import React from 'react';
import ExpandableEditor from '../../../../Common/Layout/ExpandableEditor/Editor';
import TypeBuilder from './TypeBuilder';

const TypeEditor = ({ type, setType, isLast, property, service, allTypes }) => {
  const editorExpanded = () => {
    return <TypeBuilder type={type} setType={setType} allTypes={allTypes} />;
  };

  const expandButtonText = isLast ? 'Create a new type' : 'Edit';
  const collapseButtonText = isLast ? 'Cancel' : 'Close';

  let collapsedLabel = null;
  if (!isLast) {
    collapsedLabel = () => (
      <div>
        <b>{type.name} </b>
        <i> - {type.kind}</i>
      </div>
    );
  }

  return (
    <ExpandableEditor
      editorExpanded={editorExpanded}
      collapsedLabel={collapsedLabel}
      property={property}
      service={service}
      expandButtonText={expandButtonText}
      collapseButtonText={collapseButtonText}
    />
  );
};

export default TypeEditor;
