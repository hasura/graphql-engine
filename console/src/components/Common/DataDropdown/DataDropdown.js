import React, { useState } from 'react';

const styles = require('./DataDropdown.scss');

const ComponentData = ({ options }) => {
  const generateOptions = () => {
    return options.map((o, i) => (
      <li key={i} onClick={o.onChange} data-test={o.itemIdentifier}>
        {`${o.prefixLabel} ${o.displayName}`}
      </li>
    ));
  };
  /*
    TODO: Implement position API
  */
  return <ul className={styles.dropdown_wrapper}>{generateOptions()}</ul>;
};

const DataDropdown = ({ elementId, children, options, position }) => {
  const [dropdownState, toggle] = useState(false);
  const toggleDropdown = () => toggle(!dropdownState);
  const showDropdown = () =>
    dropdownState && <ComponentData position={position} options={options} />;
  return (
    <div testId={`${elementId}`} className={styles.data_dropdown_wrapper}>
      <span onClick={toggleDropdown}>
        {children}
        {showDropdown()}
      </span>
    </div>
  );
};

export default DataDropdown;
