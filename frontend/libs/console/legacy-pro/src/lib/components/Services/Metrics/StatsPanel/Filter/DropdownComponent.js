import React, { useState } from 'react';
import Dropdown from 'react-bootstrap/lib/Dropdown';
import MenuItem from 'react-bootstrap/lib/MenuItem';

import { SELECT_ALL_NAME_DISPLAY } from '../../constants';

const dropdown = require('../../images/drop-down.svg');

const DropdownComponent = props => {
  /* Accepts children which can be a single Menu Items
   * or array of Menu Items
   * TODO: How to validate whether the childrens are only of Menu item type?
   * */
  const [show, setShow] = useState(false);
  const {
    id,
    displayValue,
    options,
    onChange,
    selectedValues,
    children,
    selectAll,
  } = props;
  const selectedValuesList =
    Object.keys(selectedValues).length === options.length - 1
      ? Object.assign({ ...selectedValues }, { 'Select All': true })
      : selectedValues;
  const styles = require('../../Metrics.scss');
  const handleToggle = (isOpen, event, metadata) => {
    if (isOpen || metadata.source !== 'select') {
      setShow(isOpen);
    }
  };
  const handleChange = val => {
    if (
      val === SELECT_ALL_NAME_DISPLAY &&
      Object.keys(selectedValuesList).length === options.length
    ) {
      selectAll(id, options, true);
    } else if (val === SELECT_ALL_NAME_DISPLAY) {
      selectAll(id, options);
    } else {
      onChange(val);
    }
  };

  const renderTitle = title => {
    if (
      title === 'Select All' &&
      Object.keys(selectedValuesList).length === options.length
    ) {
      return 'Unselect All';
    }
    return title;
  };

  return (
    <Dropdown
      open={show}
      id="dropdown-custom-menu"
      onToggle={handleToggle}
      onSelect={e => {
        handleChange(e);
      }}
      className={styles.dropDownBtn}
    >
      <Dropdown.Toggle noCaret>
        {displayValue}
        <img src={dropdown} alt={'Dropdown arrow'} />
      </Dropdown.Toggle>
      <Dropdown.Menu>
        {options.map((list, index) => {
          return (
            <MenuItem key={displayValue + '_' + index} eventKey={list.title}>
              <div className={styles.commonCheckBox}>
                <input
                  id={id + '_' + index}
                  type="checkbox"
                  className="legacy-input-fix"
                  value={list.title}
                  onChange={() => null}
                  checked={selectedValuesList[list.title] ? true : false}
                />
                <label
                  className={styles.eclipseText + ' ' + styles.fw_light}
                  htmlFor={id + '_' + index}
                >
                  {renderTitle(list.title)}
                </label>
              </div>
            </MenuItem>
          );
        })}
        {children}
      </Dropdown.Menu>
    </Dropdown>
  );
};
export default DropdownComponent;
