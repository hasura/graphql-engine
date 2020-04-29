import React from 'react';
import AceEditor from 'react-ace';

import { checkConstraintExpression } from '../TooltipMessages';
import { ToolTip, Text, Link } from '../../../../UIKit/atoms';
import styles from '../../../../Common/Common.scss';

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
        <Text fontWeight="bold" mb="sm">
          Constraint Name:
        </Text>
        <input
          type="text"
          value={name}
          onChange={nameOnChange}
          className={`form-control ${styles.wd50percent}`}
        />
      </div>
      <div>
        <Text fontWeight="bold" mb="sm">
          Check Expression:
          <ToolTip message={checkConstraintExpression} ml="sm" mr="20px" />
        </Text>
        <Link
          type="moreInfo"
          href="https://www.postgresql.org/docs/current/ddl-constraints.html#DDL-CONSTRAINTS-CHECK-CONSTRAINTS"
        />
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
