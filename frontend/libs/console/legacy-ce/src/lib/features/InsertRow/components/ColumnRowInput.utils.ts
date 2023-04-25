import { format } from 'date-fns';
import { SupportedDrivers } from '../../hasura-metadata-types';

const DEFAULT_TIME_FORMAT = 'HH:mm:ss';
const DEFAULT_TIME_WITH_TIME_ZONE_FORMAT = 'HH:mm:ssx';

const DEFAULT_DATETIME_FORMAT = 'yyyy-MM-dd HH:mm:ss';
const MYSQL_DATETIME_FORMAT = 'yyyy-MM-dd HH:mm:ss';

const DEFAULT_TIMESTAMP_WITHOUT_TIME_ZONE_FORMAT = 'yyyy-MM-dd HH:mm:ss';
const POSTGRES_TIMESTAMP_WITHOUT_TIME_ZONE_FORMAT = 'yyyy-MM-dd HH:mm:ss';

const DEFAULT_TIMESTAMP_WITH_TIME_ZONE_FORMAT = 'yyyy-MM-dd HH:mm:ssx';
const POSTGRES_TIMESTAMP_WITH_TIME_ZONE_FORMAT = 'yyyy-MM-dd HH:mm:ssx';

type DataTypeFormat =
  | 'datetime'
  | 'timestamp_without_time_zone'
  | 'timestamp_with_time_zone'
  | 'time_with_time_zone'
  | 'time';

const formats: Record<SupportedDrivers, Record<DataTypeFormat, string>> = {
  mysql8: {
    datetime: MYSQL_DATETIME_FORMAT,
    time_with_time_zone: DEFAULT_TIME_WITH_TIME_ZONE_FORMAT,
    timestamp_without_time_zone: DEFAULT_TIMESTAMP_WITHOUT_TIME_ZONE_FORMAT,
    timestamp_with_time_zone: DEFAULT_TIMESTAMP_WITH_TIME_ZONE_FORMAT,
    time: DEFAULT_TIME_FORMAT,
  },
  postgres: {
    datetime: DEFAULT_DATETIME_FORMAT,
    time_with_time_zone: DEFAULT_TIME_WITH_TIME_ZONE_FORMAT,
    timestamp_without_time_zone: POSTGRES_TIMESTAMP_WITHOUT_TIME_ZONE_FORMAT,
    timestamp_with_time_zone: POSTGRES_TIMESTAMP_WITH_TIME_ZONE_FORMAT,
    time: DEFAULT_TIME_FORMAT,
  },
  '': {
    datetime: DEFAULT_DATETIME_FORMAT,
    time_with_time_zone: DEFAULT_TIME_WITH_TIME_ZONE_FORMAT,
    timestamp_without_time_zone: POSTGRES_TIMESTAMP_WITHOUT_TIME_ZONE_FORMAT,
    timestamp_with_time_zone: POSTGRES_TIMESTAMP_WITH_TIME_ZONE_FORMAT,
    time: DEFAULT_TIME_FORMAT,
  },
};

type DataType =
  | 'datetime'
  | 'timestamp without time zone'
  | 'timestamp with time zone'
  | 'timestamp'
  | 'time'
  | 'time with time zone'
  | 'time without time zone';

export const getFormatDateFn = (
  dataType: DataType,
  driver: SupportedDrivers
) => {
  if (dataType === 'datetime') {
    const columnFormat = formats[driver]?.datetime || DEFAULT_DATETIME_FORMAT;
    return (date: Date) => format(date, columnFormat);
  }

  if (dataType === 'timestamp without time zone') {
    return (date: Date) => {
      const columnFormat =
        formats[driver]?.timestamp_without_time_zone ||
        DEFAULT_TIMESTAMP_WITHOUT_TIME_ZONE_FORMAT;
      return format(date, columnFormat);
    };
  }

  if (dataType === 'timestamp with time zone') {
    return (date: Date) => {
      const columnFormat =
        formats[driver]?.timestamp_with_time_zone ||
        DEFAULT_TIMESTAMP_WITH_TIME_ZONE_FORMAT;
      return format(date, columnFormat);
    };
  }

  if (dataType === 'timestamp') {
    return (date: Date) => {
      const columnFormat =
        formats[driver]?.timestamp_with_time_zone ||
        DEFAULT_TIMESTAMP_WITH_TIME_ZONE_FORMAT;
      return format(date, columnFormat);
    };
  }

  if (dataType === 'time' || dataType === 'time without time zone') {
    const columnFormat = formats[driver]?.time || DEFAULT_TIME_FORMAT;
    return (date: Date) => format(date, columnFormat);
  }

  if (dataType === 'time with time zone') {
    const columnFormat =
      formats[driver]?.time_with_time_zone ||
      DEFAULT_TIME_WITH_TIME_ZONE_FORMAT;
    return (date: Date) => format(date, columnFormat);
  }

  return (date: Date) => date.toISOString();
};
