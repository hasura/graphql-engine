import React from 'react';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import {
  setCheckConstraints,
  saveCheckConstraint,
  removeCheckConstraint,
} from './ModifyActions';
import { ConstraintExpandedContent } from '../Common/Components/ConstraintExpandedContent';
import { getCheckConstraintBoolExp } from '../../../../dataSources';

const CheckConstraints = ({
  constraints,
  dispatch,
  checkConstraintsModify,
  readOnlyMode,
}) => {
  const init = () => {
    const checkConstraintsState = constraints.map(c => ({
      name: c.constraint_name,
      check: getCheckConstraintBoolExp(c.check),
    }));

    checkConstraintsState.push({ name: '', check: '' });

    dispatch(setCheckConstraints(checkConstraintsState));
  };
  React.useEffect(init, [constraints]);

  const setCheckConstraint = (c, index) => {
    const newConstraints = JSON.parse(JSON.stringify(checkConstraintsModify));
    newConstraints[index] = c;
    dispatch(setCheckConstraints(newConstraints));
  };

  // map over constraints
  return checkConstraintsModify.map((constraint, i) => {
    const nameOnChange = e => {
      setCheckConstraint(
        {
          ...constraint,
          name: e.target.value,
        },
        i
      );
    };

    const checkOnChange = v => {
      setCheckConstraint(
        {
          ...constraint,
          check: v,
        },
        i
      );
    };

    const isLast = constraints.length <= i;

    const { name, check } = constraint;

    const existingConstraintName = isLast
      ? 'new-constraint'
      : constraints[i].constraint_name;

    // constraint name as collapsed label
    const collapsedLabel = () => {
      if (isLast) {
        if (!constraints.length) {
          return <div>No check constraints</div>;
        }
        return null;
      }

      return (
        <div>
          <b>{existingConstraintName}</b>
        </div>
      );
    };

    // constraint name as expanded label
    const expandedLabel = () => {
      if (isLast) {
        return null;
      }

      return (
        <div>
          <b>{existingConstraintName}</b>
        </div>
      );
    };

    // expand button text "View"
    const expandButtonText = isLast
      ? constraints.length
        ? 'Add a new check constraint'
        : 'Add'
      : 'Edit';

    // Check constraint definition in AceEditor for syntax highlighting

    let saveFunc;
    if (name && check) {
      saveFunc = toggle => {
        dispatch(saveCheckConstraint(i, toggle));
      };
    }

    // function to remove the check constraint
    let removeFunc;
    if (!isLast) {
      removeFunc = toggle => {
        dispatch(removeCheckConstraint(existingConstraintName, toggle));
      };
    }

    const expandedContent = () => (
      <ConstraintExpandedContent
        nameOnChange={nameOnChange}
        constraintName={existingConstraintName}
        name={name}
        checkOnChange={checkOnChange}
        check={check}
      />
    );

    return (
      <ExpandableEditor
        key={existingConstraintName}
        editorExpanded={expandedContent}
        expandedLabel={expandedLabel}
        collapsedLabel={collapsedLabel}
        property={`check-constraint-${i}`}
        service="modify-table"
        expandButtonText={expandButtonText}
        removeFunc={removeFunc}
        saveFunc={saveFunc}
        isCollapsable
        readOnlyMode={readOnlyMode}
      />
    );
  });
};

export default CheckConstraints;
