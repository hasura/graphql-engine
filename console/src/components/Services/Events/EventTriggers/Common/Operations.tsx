import React from 'react';
import PropTypes from 'prop-types';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import * as tooltip from './Tooltips';
import KnowMoreLink from '../../../../Common/KnowMoreLink/KnowMoreLink';
import { capitalize } from '../../../../Common/utils/jsUtils';
import styles from '../TableCommon/EventTable.scss';
import { EVENT_TRIGGER_OPERATIONS } from '../../constants';
import { EventTriggerOperation  } from '../../Types';

type OperationProps = {
  selectedOperations: Record <EventTriggerOperation, boolean>,
  setOperations: (o: Record <EventTriggerOperation, boolean>) => void,
  readOnly: boolean
}

const Operations = ({
  selectedOperations,
  setOperations,
  readOnly
}: OperationProps) => {

  const setOperation = (e: React.BaseSyntheticEvent) => {
    const label: EventTriggerOperation = e.target.name;
    const checked = e.target.checked;
    setOperations({
      ...selectedOperations,
      [label]: checked
    })
  }
 
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
          <KnowMoreLink href="https://hasura.io/docs/1.0/graphql/manual/event-triggers/invoke-trigger-console.html" />
        </span>
      ) : (
        capitalize(o)
      ),
  }));

  const getOperationsList = () => {
    return allOperations.map((o, i) => (
      <div
        key={i}
        className={`${styles.display_inline} ${styles.add_mar_right}`}
      >
        <label>
          <input
            onChange={o.onChange}
            data-test={o.testIdentifier}
            className={`${styles.display_inline} ${styles.add_mar_right}`}
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

  return (
    <div className={styles.add_mar_left_small}>{getOperationsList()}</div>
  );
};

export default Operations;
