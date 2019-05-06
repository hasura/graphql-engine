import React from 'react';
import PropTypes from 'prop-types';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import * as tooltip from './Tooltips';
import { TOGGLE_ENABLE_MANUAL_CONFIG } from './AddActions';

/*
 * Generates a list of operations on the UI
 * Accepts an array of objects of the form
 *  {
 *    name: <name of the operation>
 *    onChange: <event when operation is interacted with>
 *    value: <current state of operation>
 *    testIdentifier: <for cypress>
 *  }
 * */

const Operations = ({
  operations,
  supportManualTriggerInvocations,
  enableManual,
  dispatch,
}) => {
  const styles = require('../TableCommon/EventTable.scss');

  const getManualInvocationOption = () => {
    const handleToggleEnableManualOperation = () => {
      dispatch({ type: TOGGLE_ENABLE_MANUAL_CONFIG });
    };

    const manualInvocation = {
      name: 'enable_manual',
      testIdentifier: 'enable-manual-operation',
      isChecked: enableManual,
      onChange: handleToggleEnableManualOperation,
      displayName: 'Enable running trigger via Data browser',
    };

    return (
      supportManualTriggerInvocations && (
        <div className={styles.manualInvocationCheckbox}>
          <label>
            <input
              className={`${styles.display_inline} ${styles.add_mar_right}`}
              type="checkbox"
              value={manualInvocation.name}
              checked={manualInvocation.isChecked}
              onChange={manualInvocation.onChange}
              data-test={manualInvocation.testIdentifier}
            />
            Via console data browser{' '}
            <a
              href="https://docs.hasura.io/graphql/manual/event-triggers/invoke-trigger-console.html"
              target="_blank"
              rel="noopener noreferrer"
            >
              <small>
                <i>(Know more)</i>
              </small>
            </a>
          </label>
        </div>
      )
    );
  };

  const operationsList = () =>
    operations.map((o, i) => (
      <div
        key={i}
        className={`${styles.display_inline} ${i !== 0 && styles.add_mar_left}`}
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

  return (
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
      <div className={styles.add_mar_left_small}>
        <div>
          <h5
            className={styles.fontWeightBold + ' ' + styles.padd_bottom_small}
          >
            Database operations &nbsp; &nbsp;
            <OverlayTrigger
              placement="right"
              overlay={tooltip.dbOperationsDescription}
            >
              <i className="fa fa-question-circle" aria-hidden="true" />
            </OverlayTrigger>{' '}
          </h5>
          {operationsList()}
        </div>
        <div className={styles.add_mar_top_small}>
          <h5
            className={styles.fontWeightBold + ' ' + styles.padd_bottom_small}
          >
            Manual operation &nbsp; &nbsp;
            <OverlayTrigger
              placement="right"
              overlay={tooltip.manualOperationsDescription}
            >
              <i className="fa fa-question-circle" aria-hidden="true" />
            </OverlayTrigger>{' '}
          </h5>
          {getManualInvocationOption()}
        </div>
      </div>
    </div>
  );
};

Operations.propTypes = {
  operations: PropTypes.array.isRequired,
  supportManualTriggerInvocations: PropTypes.bool.isRequired,
  enableManual: PropTypes.bool.isRequired,
};

export default Operations;
