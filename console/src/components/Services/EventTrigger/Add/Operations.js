import React from 'react';
import PropTypes from 'prop-types';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';

import * as tooltip from './Tooltips';
import { TOGGLE_ENABLE_MANUAL_CONFIG } from './AddActions';
import KnowMoreLink from '../../../Common/KnowMoreLink/KnowMoreLink';
import { Icon } from '../../../UIKit/atoms';
import styles from '../TableCommon/EventTable.scss';

const Operations = ({
  enableManual,
  selectedOperations,
  handleOperationSelection,
  dispatch,
}) => {
  const databaseOperations = [
    {
      name: 'insert',
      testIdentifier: 'insert-operation',
      isChecked: selectedOperations.insert,
      onChange: handleOperationSelection,
      displayName: 'Insert',
    },
    {
      name: 'update',
      testIdentifier: 'update-operation',
      isChecked: selectedOperations.update,
      onChange: handleOperationSelection,
      displayName: 'Update',
    },
    {
      name: 'delete',
      testIdentifier: 'delete-operation',
      isChecked: selectedOperations.delete,
      onChange: handleOperationSelection,
      displayName: 'Delete',
    },
  ];

  const getManualInvokeOperation = () => {
    const handleManualOperationSelection = () => {
      dispatch({ type: TOGGLE_ENABLE_MANUAL_CONFIG });
    };

    return {
      name: 'enable_manual',
      testIdentifier: 'enable-manual-operation',
      isChecked: enableManual,
      onChange: handleManualOperationSelection,
      displayName: (
        <span>
          Via console &nbsp;&nbsp;
          <OverlayTrigger
            placement="right"
            overlay={tooltip.manualOperationsDescription}
          >
            <Icon type="questionCircle" size={12} ml="xs" />
          </OverlayTrigger>
          &nbsp;&nbsp;
          <KnowMoreLink href="https://hasura.io/docs/1.0/graphql/manual/event-triggers/invoke-trigger-console.html" />
        </span>
      ),
    };
  };

  const getOperationsList = () => {
    const manualOperation = getManualInvokeOperation();

    const allOperations = databaseOperations;
    if (manualOperation) {
      allOperations.push(manualOperation);
    }

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
            value={o.name}
            checked={o.isChecked}
          />
          {o.displayName}
        </label>
      </div>
    ));
  };

  return (
    <div>
      <div className={styles.add_mar_bottom + ' ' + styles.selectOperations}>
        <h4 className={styles.subheading_text}>
          Trigger Operations &nbsp; &nbsp;
          <OverlayTrigger
            placement="right"
            overlay={tooltip.operationsDescription}
          >
            <Icon type="questionCircle" size={12} ml="xs" />
          </OverlayTrigger>{' '}
        </h4>
        <div className={styles.add_mar_left_small}>{getOperationsList()}</div>
      </div>
    </div>
  );
};

Operations.propTypes = {
  enableManual: PropTypes.bool.isRequired,
  selectedOperations: PropTypes.object.isRequired,
  handleOperationSelection: PropTypes.func.isRequired,
};

export default Operations;
