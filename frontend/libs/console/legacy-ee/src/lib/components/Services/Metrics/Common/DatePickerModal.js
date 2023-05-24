import { useState } from 'react';
import { Dialog } from '@hasura/console-legacy-ce';
// import DatePicker from 'react-datepicker';
// import 'react-datepicker/dist/react-datepicker.css';
import { DateRangePicker } from 'react-date-range';
import 'react-date-range/dist/styles.css'; // main style file
import 'react-date-range/dist/theme/default.css'; // theme css file
// import moment from 'moment';
import { Button } from '@hasura/console-legacy-ce';

import styles from '../Metrics.module.scss';
import clsx from 'clsx';

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
  if (!show) return null;

  return (
    <Dialog
      hasBackdrop
      id="dateModal"
      onClose={onHide}
      title="Custom Date"
      portal
    >
      <div
        className={clsx(
          styles.datePickerModalWrapper,
          '!pointer-events-auto',
          'p-sm'
        )}
      >
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
              rangeColors={['var(--tw-color-amber-500-hex)']}
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
      </div>
    </Dialog>
  );
};

export default DatePickerModal;
