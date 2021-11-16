import React from 'react';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import * as tooltip from './Tooltips';
import KnowMoreLink from '../../../../Common/KnowMoreLink/KnowMoreLink';
import { capitalize } from '../../../../Common/utils/jsUtils';
import styles from '../TableCommon/EventTable.scss';
import { EVENT_TRIGGER_OPERATIONS } from '../../constants';
import { EventTriggerOperation } from '../../types';

type OperationProps = {
  selectedOperations: Record<EventTriggerOperation, boolean>;
  setOperations: (o: Record<EventTriggerOperation, boolean>) => void;
  readOnly: boolean;
};

const Operations = ({
  selectedOperations,
  setOperations,
  readOnly,
}: OperationProps) => {
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
        <span>
          Via console &nbsp;&nbsp;
          <OverlayTrigger
            placement="right"
            overlay={tooltip.manualOperationsDescription}
          >
            <i className="fa fa-question-circle" aria-hidden="true" />
          </OverlayTrigger>
          &nbsp;&nbsp;
          <KnowMoreLink href="https://hasura.io/docs/latest/graphql/core/event-triggers/invoke-trigger-console.html" />
        </span>
      ) : (
        capitalize(o)
      ),
  }));

  const getOperationsList = () => {
    return allOperations.map(o => (
      <div
        key={o.name}
        className={`${styles.display_inline} ${styles.add_mar_right}`}
      >
        <label className={styles.cursorPointer}>
          <input
            onChange={o.onChange}
            data-test={o.testIdentifier}
            className={`${styles.display_inline} ${styles.add_mar_right_small} ${styles.cursorPointer} legacy-input-fix`}
            type="checkbox"
            name={o.name}
            checked={o.isChecked}
            disabled={readOnly}
          />
          {o.displayName}
        </label>
      </div>
    ));
  };

  return <div>{getOperationsList()}</div>;
};

export default Operations;
