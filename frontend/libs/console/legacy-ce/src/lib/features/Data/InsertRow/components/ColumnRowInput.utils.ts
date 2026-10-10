import { format } from 'date-fns';
import { SupportedDriver } from '@hasura/shared/types';

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

const getFormats = (
  driver: SupportedDriver,
): Record<DataTypeFormat, string> => {
  switch (driver) {
    case 'mysql':
    case 'mariadb':
      return {
        datetime: MYSQL_DATETIME_FORMAT,
        time_with_time_zone: DEFAULT_TIME_WITH_TIME_ZONE_FORMAT,
        timestamp_without_time_zone: DEFAULT_TIMESTAMP_WITHOUT_TIME_ZONE_FORMAT,
        timestamp_with_time_zone: DEFAULT_TIMESTAMP_WITH_TIME_ZONE_FORMAT,
        time: DEFAULT_TIME_FORMAT,
      };
    default:
      return {
        datetime: DEFAULT_DATETIME_FORMAT,
        time_with_time_zone: DEFAULT_TIME_WITH_TIME_ZONE_FORMAT,
        timestamp_without_time_zone:
          POSTGRES_TIMESTAMP_WITHOUT_TIME_ZONE_FORMAT,
        timestamp_with_time_zone: POSTGRES_TIMESTAMP_WITH_TIME_ZONE_FORMAT,
        time: DEFAULT_TIME_FORMAT,
      };
  }
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
  driver: SupportedDriver,
) => {
  const formats = getFormats(driver);
  if (dataType === 'datetime') {
    const columnFormat = formats.datetime;
    return (date: Date) => format(date, columnFormat);
  }

  if (dataType === 'timestamp without time zone') {
    return (date: Date) => {
      const columnFormat = formats.timestamp_without_time_zone;
      return format(date, columnFormat);
    };
  }

  if (dataType === 'timestamp with time zone') {
    return (date: Date) => {
      const columnFormat = formats.timestamp_with_time_zone;
      return format(date, columnFormat);
    };
  }

  if (dataType === 'timestamp') {
    return (date: Date) => {
      const columnFormat = formats.timestamp_with_time_zone;
      return format(date, columnFormat);
    };
  }

  if (dataType === 'time' || dataType === 'time without time zone') {
    const columnFormat = formats.time;
    return (date: Date) => format(date, columnFormat);
  }

  if (dataType === 'time with time zone') {
    const columnFormat = formats.time_with_time_zone;
    return (date: Date) => format(date, columnFormat);
  }

  return (date: Date) => date.toISOString();
};
