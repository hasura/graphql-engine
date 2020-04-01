import React, { useEffect, useState } from 'react';

import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import { removeCheckConstraint, setCheckConstraints } from './AddActions';
import { ConstraintExpandedContent } from '../Common/Components/ConstraintExpandedContent';

const CheckConstraints = ({ dispatch, constraints }) => {
  const [addConstraintsState, setAddConstraintsState] = useState([]);

  useEffect(() => {
    const newConstraintsState = [...constraints, { name: '', check: '' }];
    setAddConstraintsState(newConstraintsState);
  }, [constraints]);

  return addConstraintsState.map(({ name, check }, i) => {
    const onChangeName = e => {
      e.persist();
      setAddConstraintsState(prev =>
        prev.map((c, idx) => {
          if (i === idx) return { ...c, name: e.target.value };
          return c;
        })
      );
    };

    const onChangeCheck = ch => {
      setAddConstraintsState(prev =>
        prev.map((c, idx) => {
          if (i === idx) return { ...c, check: ch };
          return c;
        })
      );
    };

    const isLast = addConstraintsState.length - 1 === i;

    const existingConstraintName = isLast
      ? 'new-constraint'
      : constraints[i] && constraints[i].name;

    const collapsedLabel = () => {
      if (isLast) {
        return addConstraintsState.length === 1 ? (
          <div>
            <i>(You can add check constraints later as well)</i>
          </div>
        ) : null;
      }

      return (
        <div>
          <b>{existingConstraintName}</b>
        </div>
      );
    };

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

    const expandButtonText = isLast ? 'Add a check constraint' : 'Edit';

    let saveFunc;
    if (name && check) {
      saveFunc = toggle => {
        dispatch(
          setCheckConstraints(
            addConstraintsState.filter(c => c.name && c.check)
          )
        );
        toggle();
      };
    }

    let removeFunc;
    if (!isLast) {
      removeFunc = toggle => {
        dispatch(removeCheckConstraint(i));
        setAddConstraintsState(prev => prev.filter((_, idx) => idx !== i));
        toggle();
      };
    }
    const expandedContent = () => (
      <ConstraintExpandedContent
        nameOnChange={onChangeName}
        constraintName={existingConstraintName}
        name={name}
        checkOnChange={onChangeCheck}
        check={check}
      />
    );

    return (
      <ExpandableEditor
        editorExpanded={expandedContent}
        expandedLabel={expandedLabel}
        collapsedLabel={collapsedLabel}
        property={`check-constraint-${i}`}
        service="modify-table"
        expandButtonText={expandButtonText}
        removeFunc={removeFunc}
        saveFunc={saveFunc}
        isCollapsable
      />
    );
  });
};

export default CheckConstraints;
