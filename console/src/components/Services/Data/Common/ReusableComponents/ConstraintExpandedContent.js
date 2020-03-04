import React from 'react';
import AceEditor from 'react-ace';

import styles from '../../../../Common/Common.scss';
import ToolTip from '../../../../Common/Tooltip/Tooltip';
import KnowMoreLink from '../../../../Common/KnowMoreLink/KnowMoreLink';
import { checkConstraintExpression } from '../TooltipMessages';

export const ConstraintExpandedContent = ({
  nameOnChange,
  constraintName,
  name,
  checkOnChange,
  check,
}) => {
  return (
    <div>
      <div className={styles.add_mar_bottom}>
        <div className={styles.add_mar_bottom_mid}>
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
        <div className={styles.add_mar_bottom_mid}>
          <b>Check Expression: </b>
          <ToolTip message={checkConstraintExpression} />
          &nbsp;&nbsp;
          <KnowMoreLink href="https://www.postgresql.org/docs/current/ddl-constraints.html#DDL-CONSTRAINTS-CHECK-CONSTRAINTS" />
        </div>
        <AceEditor
          mode="sql"
          theme="github"
          name={constraintName}
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
