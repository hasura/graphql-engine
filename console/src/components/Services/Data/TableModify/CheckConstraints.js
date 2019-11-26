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
import ToolTip from '../../../Common/Tooltip/Tooltip';
import KnowMoreLink from '../../../Common/KnowMoreLink/KnowMoreLink';
import { getCheckConstraintBoolExp } from '../../../Common/utils/sqlUtils';

const CheckConstraints = ({
  constraints,
  dispatch,
  checkConstraintsModify,
}) => {
  const init = () => {
    const checkConstraintsState = constraints.map(c => ({
      name: getCheckConstraintName(c),
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
      : getCheckConstraintName(constraints[i]);

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
    // eslint-disable-next-line no-nested-ternary
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
              <b>Check Expression: </b>
              <ToolTip
                message={
                  'Boolean expression that must be satisfied for all rows in the table. e.g. min_price >= 0 AND max_price >= min_price'
                }
              />
              &nbsp;&nbsp;
              <KnowMoreLink href="https://www.postgresql.org/docs/current/ddl-constraints.html#DDL-CONSTRAINTS-CHECK-CONSTRAINTS" />
            </div>
            <AceEditor
              mode="sql"
              theme="github"
              // placeholder="email ~ '^[A-Za-z0-9._%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'"
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
