import { format } from 'date-fns';
import { columnDataType, TableColumn } from '@hasura/metadata/data-source';

const DATE_FORMATS = {
  timestamp: 'yyyy-MM-dd HH:mm:ss',
  time: 'HH:mm:ss',
};

/**
 * Whether a timestamp or time column keeps a time zone offset, e.g.
 * `timestamptz`, `time with time zone`, `datetimeoffset`, `timestamp_ltz`
 * (but not `timestamp_ntz`) or the Data Connectors' `datetime_with_timezone`.
 */
export const hasTimeZone = (dataType: TableColumn['dataType']) =>
  /with[ _]time ?zone|offset|(^|[^n])tz$/i.test(columnDataType(dataType));

export const getFormatDateFn = (
  consoleDataType: keyof typeof DATE_FORMATS,
  withTimeZone: boolean,
) => {
  const columnFormat = `${DATE_FORMATS[consoleDataType]}${withTimeZone ? 'x' : ''}`;
  return (date: Date) => format(date, columnFormat);
};
