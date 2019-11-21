import React from 'react';
import AceEditor from 'react-ace';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import { getCheckConstraintName } from '../../../Common/utils/pgUtils';
import styles from './ModifyTable.scss';
import {
  setCheckConstraints,
  saveCheckConstraint,
  removeCheckConstraint,
} from './ModifyActions';
import { getCheckConstraintsState } from './utils';

const CheckConstraints = ({
  constraints,
  dispatch,
  checkConstraintsModify,
}) => {
  const init = () => {
    const checkConstraintsState = getCheckConstraintsState(constraints);
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
      : getCheckConstraintName(constraints[i]);

    // constraint name as expanded and collapsed label
    const label = () => {
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

    // expand button text "View"
    const expandButtonText = isLast
      ? constraints.length
        ? 'Add a new check constraint'
        : 'Add'
      : 'Edit';

    // Check constraint definition in AceEditor for syntax highlighting
    const expandedContent = () => {
      return (
        <div>
          <div className={`${styles.add_mar_bottom}`}>
            <div className={`${styles.add_mar_bottom_mid}`}>
              <b>Constraint Name:</b>
            </div>
            <input
              type="text"
              value={name}
              onChange={nameOnChange}
              className={`form-control ${styles.wd50percent}`}
            />
          </div>
          <div>
            <div className={`${styles.add_mar_bottom_mid}`}>
              <b>Boolean expression:</b>
            </div>
            <AceEditor
              mode="sql"
              theme="github"
              placeholder="email ~ '^[A-Za-z0-9._%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'"
              name={existingConstraintName}
              onChange={checkOnChange}
              value={check}
              minLines={1}
              maxLines={100}
              fontSize={15}
              width="100%"
              showPrintMargin={false}
            />
          </div>
        </div>
      );
    };

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

    return (
      <ExpandableEditor
        key={existingConstraintName}
        editorExpanded={expandedContent}
        expandedLabel={label}
        collapsedLabel={label}
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
