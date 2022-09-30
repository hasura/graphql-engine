import React, { useState } from 'react';
import DatePickerModal from '../DatePickerModal';
import MenuItem from 'react-bootstrap/lib/MenuItem';

const defaultState = {
  isDatePicker: false,
};

const CustomDatePickerMenuItem = ({ onApply, title, value }) => {
  const styles = require('../../Metrics.scss');
  const [customDate, modalToggle] = useState(defaultState);
  const { isDatePicker } = customDate;
  const modalOpen = () => {
    modalToggle({ isDatePicker: true });
  };
  const onClose = () => {
    modalToggle({ isDatePicker: false });
  };
  const onApplyClick = data => {
    onClose();
    onApply(data);
  };
  return (
    <MenuItem eventKey={value}>
      <div>
        <div className={styles.customDates} onClick={modalOpen}>
          {title}
        </div>
        <DatePickerModal
          show={isDatePicker}
          onHide={onClose}
          onApply={onApplyClick}
        />
      </div>
    </MenuItem>
  );
};

export default CustomDatePickerMenuItem;
