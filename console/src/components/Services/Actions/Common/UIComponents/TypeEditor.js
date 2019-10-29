import React from 'react';
import ExpandableEditor from '../../../../Common/Layout/ExpandableEditor/Editor';
import TypeBuilder from './TypeBuilder';

const TypeEditor = ({
  type,
  setType,
  setTypes,
  removeType,
  isLast,
  service,
  index,
  allTypes,
}) => {
  const editorExpanded = () => {
    return (
      <TypeBuilder
        type={type}
        setType={setType}
        setTypes={setTypes}
        allTypes={allTypes}
      />
    );
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

  let removeFunc;
  if (!isLast) {
    removeFunc = toggle => {
      toggle();
      removeType();
    };
  }

  return (
    <ExpandableEditor
      editorExpanded={editorExpanded}
      collapsedLabel={collapsedLabel}
      removeFunc={removeFunc}
      property={`type-editor-${index}`}
      service={service}
      expandButtonText={expandButtonText}
      collapseButtonText={collapseButtonText}
    />
  );
};

export default TypeEditor;
