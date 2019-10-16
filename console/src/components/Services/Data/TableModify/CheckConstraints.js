import React from 'react';
import AceEditor from 'react-ace';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import styles from './ModifyTable.scss';
import { removeCheckConstraint } from './ModifyActions';

const CheckConstraints = ({ constraints, dispatch }) => {
  // if no constraints are present
  if (!constraints.length) {
    return 'No check constraints';
  }

  // map over constraints
  return constraints.map((constraint, i) => {
    const { constraint_name, check } = constraint;

    // constraint name as expanded and collapsed label
    const label = () => {
      return (
        <div>
          <b>{constraint_name}</b>
        </div>
      );
    };

    // expand button text "View"
    const expandButtonText = 'View';

    // Check constraint definition in AceEditor for syntax highlighting
    const expandedContent = () => {
      return (
        <AceEditor
          mode="sql"
          theme="github"
          name={constraint_name}
          value={check}
          minLines={1}
          maxLines={100}
          width="100%"
          showPrintMargin={false}
          className={styles.add_mar_top_small}
        />
      );
    };

    // function to remove the check constraint
    const removeFunc = () => {
      dispatch(removeCheckConstraint(constraint_name));
    };

    return (
      <ExpandableEditor
        key={constraint_name}
        editorExpanded={expandedContent}
        expandedLabel={label}
        collapsedLabel={label}
        property={`check-constraint-${i}`}
        service="modify-table"
        expandButtonText={expandButtonText}
        removeFunc={removeFunc}
        isCollapsable
      />
    );
  });
};

export default CheckConstraints;
