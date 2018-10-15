/**
 * These are the list of warning and lies...
 */

export const ATLEAST_ONE_PRIMARY_KEY_MSG =
  'You should have atleast one column as a primary key.';
export const primaryKeyAlreadyPresentMsg = key =>
  `You key [${key}] is already present in the current set of primary keys.`;
export const ATLEAST_ONE_COLUMN_MSG =
  'You need atleast one column for a given table.';
export const RECOMMENEDED_NAMING_CONVENTION =
  'We recommend using the snake_case naming convertion for table, column names. E.g.: article_author.';
export const fieldRepeatedMsg = list =>
  `You have the following column names repeated: [${list}]`;
