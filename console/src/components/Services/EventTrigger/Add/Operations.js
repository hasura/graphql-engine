import React from 'react';
import PropTypes from 'prop-types';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import * as tooltip from './Tooltips';
import { TOGGLE_ENABLE_MANUAL_CONFIG } from './AddActions';

const Operations = ({
  enableManual,
  selectedOperations,
  handleOperationSelection,
  dispatch,
}) => {
  const styles = require('../TableCommon/EventTable.scss');

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
            <i className="fa fa-question-circle" aria-hidden="true" />
          </OverlayTrigger>
          &nbsp;&nbsp;
          <a
            href="https://docs.hasura.io/1.0/graphql/manual/event-triggers/invoke-trigger-console.html"
            target="_blank"
            rel="noopener noreferrer"
          >
            <small>
              <i>(Know more)</i>
            </small>
          </a>
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
            <i className="fa fa-question-circle" aria-hidden="true" />
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
