import React, { useEffect, useState } from 'react';
import AceEditor from 'react-ace';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import styles from '../TableModify/ModifyTable.scss';
import { removeCheckConstraint, setCheckConstraints } from './AddActions';
import ToolTip from '../../../Common/Tooltip/Tooltip';
import KnowMoreLink from '../../../Common/KnowMoreLink/KnowMoreLink';

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

    // constraint name as collapsed label
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
      ? addConstraintsState.length
        ? 'Add a check constraint'
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
              onChange={onChangeName}
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
              name={existingConstraintName}
              onChange={onChangeCheck}
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
