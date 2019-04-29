import React, { useState } from 'react';

import { getMeParentNode } from '../../../utils/domFunctions';

const styles = require('./DataDropdown.scss');

const ComponentData = ({ options }) => {
  const generateOptions = () => {
    return options.map((o, i) => (
      <li
        key={i}
        onClick={() => o.onChange.call(undefined, o.displayName, o.rowIndex)}
        data-test={o.itemIdentifier}
      >
        {`${o.prefixLabel} ${o.displayName}`}
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

const DataDropdown = ({
  identifier,
  elementId,
  children,
  options,
  position,
}) => {
  const [isOpen, updateState] = useState(false);
  const showDropdown = () =>
    isOpen && <ComponentData position={position} options={options} />;

  const nodeId = `data-dropdown-element_${elementId}`;

  const cb = d => e => {
    /*
     * Update the state only if the element clicked on is not the data dropdown component
     * */
    const dataElement = getMeParentNode(e.target, 'data-element');
    if (d) {
      /* If the element has parent whose `nodeId` is same as the current one
       * */
      if (dataElement && dataElement.getAttribute('data-element') === nodeId) {
        return;
      }
      updateState(!d);
    }
  };
  return (
    <div
      key={`${identifier}_wrapper`}
      data-test={`${elementId}`}
      className={styles.data_dropdown_wrapper}
      data-element={nodeId}
    >
      <span key={`${identifier}_children_wrapper`}>
        <span
          key={`${identifier}_children`}
          onClick={e => {
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
          }}
        >
          {children}
        </span>
        {showDropdown()}
      </span>
    </div>
  );
};

export default DataDropdown;
