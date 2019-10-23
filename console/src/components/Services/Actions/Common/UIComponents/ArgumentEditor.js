import React from 'react';
import styles from './Styles.scss';
import ExpandableEditor from '../../../../Common/Layout/ExpandableEditor/Editor';
import ArgumentInput from './ArgumentInput';
import HiddenMore from '../../../../Common/HiddenMore';
import TypeBuilder from './TypeBuilder';

const ArgumentEditor = ({
  argument,
  isLast,
  types,
  service,
  property,
  setArgument,
  setTypes,
}) => {
  const editorExpanded = () => {
    const typeCreator = () => {
      if (!service.includes('create')) return null;
      const typeBuilder = <TypeBuilder types={types} setTypes={setTypes} />;
      return (
        <HiddenMore
          title="Create a new type"
          expanded={false}
          more={typeBuilder}
        />
      );
    };

    return (
      <div>
        <div className={styles.add_mar_bottom}>
          <ArgumentInput
            argument={argument}
            setArgument={setArgument}
            types={types}
          />
        </div>
        {typeCreator()}
      </div>
    );
  };

  const expandButtonText = isLast ? 'Add an argument' : 'Edit';
  const collapseButtonText = isLast ? 'Cancel' : 'Close';

  return (
    <ExpandableEditor
      editorExpanded={editorExpanded}
      collapsedLabel={() => 'ArgumentName (Type)'}
      property={property}
      service={service}
      expandButtonText={expandButtonText}
      collapseButtonText={collapseButtonText}
    />
  );
};

export default ArgumentEditor;
