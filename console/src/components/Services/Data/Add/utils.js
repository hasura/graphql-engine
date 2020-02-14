const CHECK_PREFIX = 'validate';

export const getCheckName = colName => `${CHECK_PREFIX}_${colName}`;
