import { useState } from 'react';
import DatePicker from 'react-datepicker';
import 'react-datepicker/dist/react-datepicker.css';
import styles from '../Metrics.module.scss';
import clsx from 'clsx';
import { Button, Dialog } from '@hasura/shared/ui';

const DatePickerModal = (props) => {
  const [dateRange, setDateRange] = useState<[Date | null, Date | null]>([
    new Date(),
    new Date(),
  ]);
  const [startDate, endDate] = dateRange;
  const { onHide, show, onApply } = props;
  const applyCustomDate = () => {
    if (!startDate || !endDate) return;
    onApply({
      start: startDate.toISOString(),
      end: endDate.toISOString(),
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
    <Dialog onClose={onHide} title="Custom Date">
      <div
        className={clsx(
          styles.datePickerModalWrapper,
          'pointer-events-auto!',
          'p-sm',
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
            <DatePicker
              inline
              selectsRange
              startDate={startDate}
              endDate={endDate}
              onChange={(dates) => setDateRange(dates)}
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
      </div>
    </Dialog>
  );
};

export default DatePickerModal;
