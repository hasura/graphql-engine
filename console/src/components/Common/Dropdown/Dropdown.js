import React, { useState } from 'react';
import PropTypes from 'prop-types';

import { getParentNodeByAttribute } from '../../../utils/domFunctions';
import Button from '../Button/Button';

const styles = require('./Dropdown.scss');

const ComponentData = ({ options }) => {
  /*
   * options is an object which has following keys
   *  callbackArguments [array]: Arguments to be sent to the onClick function
   *  buttonText: Fills the action button text
   *  displayText: Fills non actionable text
   *  testId: Fills `data-test` attribute
   *  onClick: Function to be called when clicked
   * */
  const generateOptions = () => {
    return options.map((o, i) => (
      <li key={i} data-test={o.testId}>
        <Button
          color="white"
          size="xs"
          data-test={`run_manual_trigger_${o.testId}`}
          onClick={() => o.onClick.apply(undefined, o.callbackArguments)}
        >
          {o.buttonText}
        </Button>
        {`${o.displayText}`}
      </li>
    ));
  };
  /*
    TODO: Implement position API
  */
  return <ul className={styles.dropdown_wrapper}>{generateOptions()}</ul>;
};

const attachEventListener = updateToggle => {
  document.addEventListener('click', updateToggle, {
    once: true,
  });
};

const removeEventListener = updateToggle => {
  document.removeEventListener('click', updateToggle);
};

/* Accepts:
 *  keyPrefix: Prefixes keys with the value
 *  testId: Tag the component with this keyPrefix. This can be consumed in tests
 *  *children*: Dropdown is tied to this element. Dropdown state is toggled based on clicks to this element.
 *  options: Line items
 *  position: TODO: Unimplemented functionality.
 *
 * */
const Dropdown = ({ keyPrefix, testId, children, options, position }) => {
  const [isOpen, updateState] = useState(false);
  const showDropdown = () =>
    isOpen && <ComponentData position={position} options={options} />;

  const nodeId = `data-dropdown-element_${testId}`;

  const cb = d => e => {
    /*
     * Update the state only if the element clicked on is not the data dropdown component
     * */
    const dataElement = getParentNodeByAttribute(e.target, 'data-element');
    if (d) {
      /* If the element has parent whose `nodeId` is same as the current one
       * */
      if (dataElement && dataElement.getAttribute('data-element') === nodeId) {
        return;
      }
      updateState(!d);
    }
  };
  const onClick = e => {
    e.stopPropagation();
    /*
     * If the dropdown is not open, attach event on body
     * */
    updateState(!isOpen);

    switch (isOpen) {
      case true:
        removeEventListener(cb(false));
        break;
      default:
        attachEventListener(cb(true));
    }
  };
  return (
    <div
      key={`${keyPrefix}_wrapper`}
      data-test={`${testId}`}
      className={styles.data_dropdown_wrapper}
      data-element={nodeId}
    >
      <span key={`${keyPrefix}_children_wrapper`}>
        <span key={`${keyPrefix}_children`}>
          {React.cloneElement(children, { onClick: onClick })}
        </span>
        {showDropdown()}
      </span>
    </div>
  );
};

Dropdown.propTypes = {
  keyPrefix: PropTypes.string,
  testId: PropTypes.string.isRequired,
  children: PropTypes.node.isRequired,
  options: PropTypes.array.isRequired,
  position: PropTypes.string,
};

export default Dropdown;
