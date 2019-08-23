import React, { useState } from 'react';
import PropTypes from 'prop-types';

import { getParentNodeByAttribute } from '../../../utils/domFunctions';

const styles = require('./Dropdown.scss');

const ComponentData = ({ options, dismiss, position }) => {
  /*
   * options is a list which has the following
   *  content: html inside each row
   *  onClick (optional): An onClick handler for each row. If this is undefined, the row is not clickable
   * */

  const generateOptions = options.map((o, i) => {
    const liStyle = o.onClick ? styles.cursorPointer : '';
    const onClick = {};
    if (o.onClick) {
      onClick.onClick = () => {
        o.onClick();
        dismiss();
      };
    }
    return (
      <li key={i} className={liStyle} {...onClick}>
        {o.content}
      </li>
    );
  });

  const dropdownPositionStyle =
    position === 'bottom' ? styles.dropdownBottom : styles.dropdownRight;

  return (
    <ul className={styles.dropdown_wrapper + ' ' + dropdownPositionStyle}>
      {generateOptions}
    </ul>
  );
};

const attachEventListener = updateToggle => {
  document.addEventListener('click', updateToggle, { once: true });
};

const removeEventListener = updateToggle => {
  document.removeEventListener('click', updateToggle);
};

/* Accepts
 *  keyPrefix: Prefixes keys with the value
 *  testId: Tag the component with this keyPrefix. This can be consumed in tests
 *  *children*: Dropdown is tied to this element. Dropdown state is toggled based on clicks to this element.
 *  options: Line items
 *  position: bottom, right (default: right)
 *
 * */
const Dropdown = ({ keyPrefix, testId, children, options, position }) => {
  const [isOpen, updateState] = useState(false);
  const showDropdown = dismissCallback =>
    isOpen && (
      <ComponentData
        position={position}
        options={options}
        dismiss={dismissCallback}
      />
    );

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

  const toggle = () => {
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

  const dismissDropdown = () => updateState(false);

  return (
    <div
      key={`${keyPrefix}_wrapper`}
      data-test={`${testId}`}
      className={styles.data_dropdown_wrapper}
      data-element={nodeId}
    >
      <div
        className={styles.dataDropdown}
        key={`${keyPrefix}_children_wrapper`}
      >
        <span key={`${keyPrefix}_children`}>
          {React.cloneElement(children, { onClick: toggle })}
        </span>
        {showDropdown(dismissDropdown)}
      </div>
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
