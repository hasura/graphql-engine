import { convertDateTimeToLocale } from '@hasura/shared/utils';

export const sanitiseRow = (column: string, row: Record<string, string>) => {
  if (column === 'created_at') {
    return convertDateTimeToLocale(row[column]);
  }
  if (column === 'scheduled_time') {
    return convertDateTimeToLocale(row[column]);
  }
  const content =
    row[column] === undefined || row[column] === null ? 'NULL' : row[column];

  return content;
};
