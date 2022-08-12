const pgCategoryCode = {
  A: 'Array types',
  B: 'Boolean types',
  C: 'Composite types',
  D: 'Date/time types',
  E: 'Enum types',
  G: 'Geometric types',
  I: 'Network address types',
  N: 'Numeric types',
  P: 'Pseudo-types',
  R: 'Range types',
  S: 'String types',
  T: 'Timespan types',
  U: 'User-defined types',
  V: 'Bit-string types',
  X: 'unknown type',
};

const topCategory = ['N', 'S', 'B', 'D', 'T'];

const restCategory = [
  ...Object.keys(pgCategoryCode).filter(p => topCategory.indexOf(p) === -1),
];

const aggCategory = [...topCategory, ...restCategory];

const serialTypes = [
  'serial,bigserial,smallserial',
  'serial,bigserial,smallserial',
  'autoincrementing integer,large autoincrementing integer, small autoincrementing integer',
  'N',
];

export { pgCategoryCode, aggCategory, serialTypes };
