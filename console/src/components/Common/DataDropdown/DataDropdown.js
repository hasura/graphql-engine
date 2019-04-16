import React, { useState } from 'react';

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

const DataDropdown = ({
  identifier,
  elementId,
  children,
  options,
  position,
}) => {
  const [dropdownState, toggle] = useState(false);
  const toggleDropdown = () => toggle(!dropdownState);
  const showDropdown = () =>
    dropdownState && <ComponentData position={position} options={options} />;
  return (
    <div
      key={`${identifier}_wrapper`}
      data-test={`${elementId}`}
      className={styles.data_dropdown_wrapper}
    >
      <span key={`${identifier}_children_wrapper`}>
        <span key={`${identifier}_children`} onClick={toggleDropdown}>
          {children}
        </span>
        {showDropdown()}
      </span>
    </div>
  );
};

export default DataDropdown;
