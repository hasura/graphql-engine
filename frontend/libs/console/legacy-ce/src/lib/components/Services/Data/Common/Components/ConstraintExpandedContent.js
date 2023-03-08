import React from 'react';
import AceEditor from 'react-ace';

import { LearnMoreLink } from '../../../../../new-components/LearnMoreLink';
import ToolTip from '../../../../Common/Tooltip/Tooltip';

import { checkConstraintExpression } from '../TooltipMessages';
import { inputStyles } from '../../constants';

export const ConstraintExpandedContent = ({
  nameOnChange,
  constraintName,
  name,
  checkOnChange,
  check,
}) => {
  return (
    <div>
      <div className="mb-md">
        <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
          Constraint Name:
        </h4>
        <input
          type="text"
          value={name}
          onChange={nameOnChange}
          className={inputStyles}
        />
      </div>
      <div>
        <div className="flex items-start">
          <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
            Check Expression:
            <ToolTip message={checkConstraintExpression} />
          </h4>
          <LearnMoreLink href="https://www.postgresql.org/docs/current/ddl-constraints.html#DDL-CONSTRAINTS-CHECK-CONSTRAINTS" />
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
          setOptions={{ useWorker: false }}
        />
      </div>
    </div>
  );
};
