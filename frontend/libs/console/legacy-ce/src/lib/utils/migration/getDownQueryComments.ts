import { RunSqlType } from '../../types';
import { getRunSqlQuery } from '../../components/Common/utils/v1QueryUtils';

export const getDownQueryComments = (
  upqueries: RunSqlType[],
  source: string
) => {
  if (Array.isArray(upqueries) && upqueries.length >= 0) {
    const comment = [
      'Could not auto-generate a down migration.',
      'Please write an appropriate down migration for the SQL below:',
      ...upqueries.map(i => i.args.sql),
      '',
    ]
      .join('\n')
      // Normalize \r\n to \n and add comments before every line
      .replace(/\r?(^|\n)(?!$)/g, '$1-- ')
      // Eliminate trailing spaces
      .replace(/ +\n/g, '\n');
    return [getRunSqlQuery(comment, source)];
  }
  // all other errors
  return [];
};
