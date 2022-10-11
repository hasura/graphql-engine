import React, { useState } from 'react';
import BootstrapModal from 'react-bootstrap/lib/Modal';
// import BootstrapModalButton from 'react-bootstrap/lib/Button';
// import DatePicker from 'react-datepicker';
// import 'react-datepicker/dist/react-datepicker.css';
import { DateRangePicker } from 'react-date-range';
import 'react-date-range/dist/styles.css'; // main style file
import 'react-date-range/dist/theme/default.css'; // theme css file
// import moment from 'moment';
import { Button } from '@hasura/console-oss';

const styles = require('../Metrics.scss');

const DatePickerModal = props => {
  const selectionRange = {
    startDate: new Date(),
    endDate: new Date(),
    key: 'selection',
  };
  const [rangeState, setRangeState] = useState(selectionRange);
  const { onHide, show, onApply } = props;
  const applyCustomDate = () => {
    onApply({
      start: rangeState.startDate.toISOString(),
      end: rangeState.endDate.toISOString(),
    });
  };
  /*
  {
  const computeMinTime = e => {
    const isSame = moment(e).isSame(startDate, 'day');
    if (isSame) {
      const mStart = moment(startDate);
      return mStart.add({ minutes: 30 }).toDate();
    }
    return moment()
      .startOf('day')
      .toDate(); // set to 12:00 am today
  };
  }
  */
  return (
    <BootstrapModal
      id="dateModal"
      onHide={onHide}
      show={show}
      className={styles.datePickerModalWrapper}
    >
      <BootstrapModal.Header className={styles.modalHeader} closeButton>
        <BootstrapModal.Title className={styles.title}>
          Custom Date
        </BootstrapModal.Title>
      </BootstrapModal.Header>
      <BootstrapModal.Body>
        <div className={styles.datePickerContainer}>
          <div className={styles.datePickerWrapper}>
            {/*
            <label>From</label>
            <DatePicker
              className={styles.datePicker}
              selected={startDate}
              onChange={date => setStartDate(date)}
              showTimeSelect
              dateFormat="MMMM d, yyyy h:mm aa"
            />
            */}
            <DateRangePicker
              ranges={[rangeState]}
              onChange={date => setRangeState(date.selection)}
              dateFormat="MMMM d, yyyy h:mm aa"
            />
          </div>
          {/*
          <div className={styles.datePickerWrapper}>
            <label>To</label>
            <DatePicker
              className={styles.datePicker}
              selected={endDate}
              minDate={startDate}
              minTime={computeMinTime(endDate)}
              maxTime={moment()
                .endOf('day')
                .toDate()}
              onChange={date => setEndDate(date)}
              showTimeSelect
              dateFormat="MMMM d, yyyy h:mm aa"
            />
          </div>
          */}
        </div>
        <div className={styles.datePickerBtnWrapper}>
          <Button mode="primary" onClick={applyCustomDate}>
            Apply
          </Button>
          <Button className={styles.defaultButton} onClick={onHide}>
            Cancel
          </Button>
        </div>
      </BootstrapModal.Body>
    </BootstrapModal>
  );
};

export default DatePickerModal;
