import React from 'react';
import { ToolTip } from '@/new-components/Tooltip';
import * as tooltip from './Tooltips';
import KnowMoreLink from '../../../../Common/KnowMoreLink/KnowMoreLink';
import { capitalize } from '../../../../Common/utils/jsUtils';
import { EVENT_TRIGGER_OPERATIONS } from '../../constants';
import { EventTriggerOperation } from '../../types';

type OperationProps = {
  selectedOperations: Record<EventTriggerOperation, boolean>;
  setOperations: (o: Record<EventTriggerOperation, boolean>) => void;
  readOnly: boolean;
  tableName: string;
};

export const Operations: React.FC<OperationProps> = ({
  selectedOperations,
  setOperations,
  readOnly,
  tableName,
}) => {
  const setOperation = (e: React.BaseSyntheticEvent) => {
    const label: EventTriggerOperation = e.target.name;
    setOperations({
      ...selectedOperations,
      [label]: !selectedOperations[label],
    });
  };

  const allOperations = EVENT_TRIGGER_OPERATIONS.map(o => ({
    name: o,
    testIdentifier: `${o}-operation`,
    isChecked: selectedOperations[o],
    onChange: setOperation,
    disabled: readOnly,
    displayName:
      o === 'enable_manual' ? (
        <span className="flex items-center">
          Via console
          <ToolTip message={tooltip.manualOperationsDescription} />
          <KnowMoreLink href="https://hasura.io/docs/latest/graphql/core/event-triggers/invoke-trigger-console.html" />
        </span>
      ) : (
        capitalize(o)
      ),
  }));

  return (
    <>
      <div className="flex items-center mb-md">
        <div className="mr-md">
          On <span className="font-semibold">{tableName}</span> table:
        </div>
        {allOperations.map(o => (
          <div key={o.name} className="mr-md">
            <label className="cursor-pointer flex">
              <input
                onChange={o.onChange}
                data-test={o.testIdentifier}
                className="cursor-pointer legacy-input-fix"
                type="checkbox"
                name={o.name}
                checked={o.isChecked}
                disabled={readOnly}
              />
              <span className="ml-xs">{o.displayName}</span>
            </label>
          </div>
        ))}
      </div>
    </>
  );
};
