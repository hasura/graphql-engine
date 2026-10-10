import { useState } from 'react';
import DatePickerModal from '../DatePickerModal';
import styles from '../../Metrics.module.scss';
import { DropdownMenu } from '@radix-ui/themes';

const defaultState = {
  isDatePicker: false,
};

const CustomDatePickerMenuItem = ({ onApply, title, value }) => {
  const [customDate, modalToggle] = useState(defaultState);
  const { isDatePicker } = customDate;
  const modalOpen = () => {
    modalToggle({ isDatePicker: true });
  };
  const onClose = () => {
    modalToggle({ isDatePicker: false });
  };
  const onApplyClick = (data) => {
    onClose();
    onApply(data);
  };
  return (
    <DropdownMenu.Item key={'CustomDatePickerMenuItem'} onSelect={modalOpen}>
      <div>
        <div className={styles.customDates}>{title}</div>
        <DatePickerModal
          show={isDatePicker}
          onHide={onClose}
          onApply={onApplyClick}
        />
      </div>
    </DropdownMenu.Item>
  );
};

export default CustomDatePickerMenuItem;
