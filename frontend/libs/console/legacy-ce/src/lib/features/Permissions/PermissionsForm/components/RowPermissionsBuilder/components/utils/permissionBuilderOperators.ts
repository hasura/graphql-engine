/* Constants */

// TODO: generate using SQL query to handle all types
export const columnOperatorsInfo = {
  _eq: {
    type: 'comparision',
    inputStructure: 'object',
  },
  _neq: {
    type: 'comparision',
    inputStructure: 'object',
  },
  _in: {
    type: 'comparision',
    inputStructure: 'array',
  },
  _nin: {
    type: 'comparision',
    inputStructure: 'array',
  },
  _gt: {
    type: 'comparision',
    inputStructure: 'object',
  },
  _lt: {
    type: 'comparision',
    inputStructure: 'object',
  },
  _gte: {
    type: 'comparision',
    inputStructure: 'object',
  },
  _lte: {
    type: 'comparision',
    inputStructure: 'object',
  },
  _ceq: {
    type: 'comparision',
    inputStructure: 'array',
    inputType: 'column',
  },
  _cne: {
    type: 'comparision',
    inputStructure: 'array',
    inputType: 'column',
  },
  _cgt: {
    type: 'comparision',
    inputStructure: 'array',
    inputType: 'column',
  },
  _clt: {
    type: 'comparision',
    inputStructure: 'array',
    inputType: 'column',
  },
  _cgte: {
    type: 'comparision',
    inputStructure: 'array',
    inputType: 'column',
  },
  _clte: {
    type: 'comparision',
    inputStructure: 'array',
    inputType: 'column',
  },
  _is_null: {
    type: 'is_null',
    inputStructure: 'object',
    inputType: 'boolean',
  },
  _like: {
    type: 'pattern_match',
    inputStructure: 'object',
  },
  _nlike: {
    type: 'pattern_match',
    inputStructure: 'object',
  },
  _ilike: {
    type: 'pattern_match',
    inputStructure: 'object',
  },
  _nilike: {
    type: 'pattern_match',
    inputStructure: 'object',
  },
  _similar: {
    type: 'pattern_match',
    inputStructure: 'object',
  },
  _nsimilar: {
    type: 'pattern_match',
    inputStructure: 'object',
  },
  _regex: {
    type: 'pattern_match',
    inputStructure: 'object',
  },
  _iregex: {
    type: 'pattern_match',
    inputStructure: 'object',
  },
  _nregex: {
    type: 'pattern_match',
    inputStructure: 'object',
  },
  _niregex: {
    type: 'pattern_match',
    inputStructure: 'object',
  },
  _contains: {
    type: 'jsonb',
    inputStructure: 'object',
  },
  _contained_in: {
    type: 'jsonb',
    inputStructure: 'object',
  },
  _has_key: {
    type: 'jsonb',
    inputStructure: 'object',
    inputType: 'character',
  },
  _has_keys_any: {
    type: 'jsonb',
    inputStructure: 'array',
    inputType: 'character',
  },
  _has_keys_all: {
    type: 'jsonb',
    inputStructure: 'array',
    inputType: 'character',
  },
  _jsonb_path_exists: {
    type: 'jsonb',
    inputStructure: 'object',
    inputType: 'character',
  },
  _jsonb_path_match: {
    type: 'jsonb',
    inputStructure: 'object',
    inputType: 'character',
  },
  _st_contains: {
    type: 'geometric',
    inputStructure: 'object',
  },
  _st_crosses: {
    type: 'geometric',
    inputStructure: 'object',
    inputType: 'json',
  },
  _st_equals: {
    type: 'geometric',
    inputStructure: 'object',
    inputType: 'json',
  },
  _st_overlaps: {
    type: 'geometric',
    inputStructure: 'object',
    inputType: 'json',
  },
  _st_touches: {
    type: 'geometric',
    inputStructure: 'object',
    inputType: 'json',
  },
  _st_within: {
    type: 'geometric',
    inputStructure: 'object',
    inputType: 'json',
  },
  _st_d_within: {
    type: 'geometric_geographic',
    inputStructure: 'object',
    inputType: 'json',
  },
  _st_intersects: {
    type: 'geometric_geographic',
    inputStructure: 'object',
    inputType: 'json',
  },
};
/* Util functions */
