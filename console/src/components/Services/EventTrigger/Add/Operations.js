import React from 'react';
import PropTypes from 'prop-types';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import * as tooltip from './Tooltips';

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

const Operations = ({ operations }) => {
  const styles = require('../TableCommon/EventTable.scss');
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
        Operations &nbsp; &nbsp;
        <OverlayTrigger
          placement="right"
          overlay={tooltip.operationsDescription}
        >
          <i className="fa fa-question-circle" aria-hidden="true" />
        </OverlayTrigger>{' '}
      </h4>
      {operationsList()}
    </div>
  );
};

Operations.propTypes = {
  operations: PropTypes.array.isRequired,
};
export default Operations;
