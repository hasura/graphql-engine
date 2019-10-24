import React from 'react';
import ExpandableEditor from '../../../../Common/Layout/ExpandableEditor/Editor';
import TypeBuilder from './TypeBuilder';

const TypeEditor = ({
  type,
  setType,
  removeType,
  isLast,
  service,
  argTypes,
  fieldTypes,
  index,
}) => {
  const editorExpanded = () => {
    return (
      <TypeBuilder
        type={type}
        setType={setType}
        argTypes={argTypes}
        fieldTypes={fieldTypes}
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

  let saveFunc;
  if (type.name) {
    saveFunc = toggle => toggle();
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
      saveFunc={saveFunc}
      removeFunc={removeFunc}
      property={`type-editor-${index}`}
      service={service}
      expandButtonText={expandButtonText}
      collapseButtonText={collapseButtonText}
    />
  );
};

export default TypeEditor;
