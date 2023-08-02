/* Checks whether a condition matches on a list of elements, if yes returns the index else -1 */
import moment from 'moment';

import {
  TIME_RANGE_BY_HOUR,
  TIME_RANGE_BY_6_HOURS,
  TIME_RANGE_BY_12_HOURS,
  TIME_RANGE_BY_DAY,
  SHOW_ONLY_ERRORS_SYMBOL,
  TIME_RANGE_SYMBOL,
  EMPTY_CLIENT_NAME_SYMBOL,
  OPERATION_NAME_SYMBOL,
  OPERATION_GROUP_NAME,
  CLIENT_NAME_SYMBOL,
  HIDE_INTROPSECTION_QUERY_SYMBOL,
} from '../constants';

import { getValuesForEmptyMap } from '../utils';

const transformTimeType = d => {
  return moment(d).fromNow();
};

const transformExecutionTime = d => {
  return Math.round(d * 1000 * 100) / 100;
};

const transformRequestResponseTime = d => {
  return Math.round(d / 10) / 100;
};

const transformedVals = {
  time: transformTimeType,
  execution_time: transformExecutionTime,
  response_size: transformRequestResponseTime,
  request_size: transformRequestResponseTime,
};

const transformExecutionTimeHeader = val => {
  return `${val} ms`;
};
const transformRequestResponseSizeHeader = val => {
  return `${val} kB`;
};

const transformedHeaderVal = {
  execution_time: transformExecutionTimeHeader,
  response_size: transformRequestResponseSizeHeader,
  request_size: transformRequestResponseSizeHeader,
};

const filterByType = (filters, type) => filters.filter(f => f.type === type);
const indexOf = (list, predicateFn) => {
  let elementIndex = -1;
  list.forEach((l, index) => {
    if (predicateFn(l)) {
      elementIndex = index;
    }
  });
  return elementIndex;
};

const getJson = (list, key) => {
  const r = {};
  list.forEach(l => (r[l[key]] = true));
  return r;
};

const stripUnderScore = val => {
  return val.split('_').join(' ');
};

const capitalize = val => {
  return `${val.charAt(0).toUpperCase()}${val.slice(1)}`;
};

const curried = sourceFn => type => value => {
  sourceFn(type, value);
};

const curriedInputElement = sourceFn => type => e => {
  if (e.keyCode === 13) {
    if (e.target.value.length > 0) {
      sourceFn(type, e.target.value);
    }
  }
};

const curriedCheckboxElement = sourceFn => type => e => {
  const getId = e.target.getAttribute('data-field-id');
  sourceFn(type, getId);
};

/* Doing this to fix refreshing state on every miliseconds change */
const removeSecondsMiliseconds = d => {
  d.setSeconds(0, 0);
  return d;
};

const getTimeRangeValue = symbol => {
  const now = new Date();
  switch (symbol) {
    case TIME_RANGE_BY_HOUR:
      const oneHourBack = new Date();
      oneHourBack.setHours(now.getHours() - 1);
      return removeSecondsMiliseconds(oneHourBack).toISOString();
    case TIME_RANGE_BY_DAY:
      const oneDayBack = new Date();
      oneDayBack.setDate(oneDayBack.getDate() - 1);
      return removeSecondsMiliseconds(oneDayBack).toISOString();
    case TIME_RANGE_BY_6_HOURS:
      const sixHoursBack = new Date();
      sixHoursBack.setHours(sixHoursBack.getHours() - 6);
      return removeSecondsMiliseconds(sixHoursBack).toISOString();
    case TIME_RANGE_BY_12_HOURS:
      const twelveHoursBack = new Date();
      twelveHoursBack.setHours(now.getHours() - 12);
      return removeSecondsMiliseconds(twelveHoursBack).toISOString();
    default:
      return new Date(0).toISOString();
  }
};

export const getWhereClause = (filterMap, filters) => {
  const fFilter = [];
  const emptyValues = getValuesForEmptyMap();
  Object.keys(filterMap).forEach(f => {
    const appliedFilters = filterByType(filters, f);
    const fil = [];
    // TODO: The below logic can be improved by using _in operator to search for something like
    // ["", null] for nullable string column. Currently it uses _or and adds each condition inplace
    appliedFilters.forEach(e => {
      if (emptyValues.indexOf(e.value) !== -1) {
        if (e.value === EMPTY_CLIENT_NAME_SYMBOL) {
          fil.push(
            {
              client_name: {
                _eq: '',
              },
            },
            {
              client_name: {
                _is_null: true,
              },
            }
          );
        } else {
          fil.push('');
        }
      } else {
        if (f === OPERATION_NAME_SYMBOL) {
          fil.push(`%${e.value}%`);
        } else {
          if (f === CLIENT_NAME_SYMBOL) {
            fil.push({
              client_name: {
                _eq: e.value,
              },
            });
          } else {
            fil.push(e.value);
          }
        }
      }
    });
    if (appliedFilters.length > 0) {
      if (f === OPERATION_NAME_SYMBOL) {
        fFilter.push({
          [f]: {
            _similar: `(${fil.join('|')})`,
          },
        });

        /*
        [f] = {};
        */
        /* If total filters is equal to one and its empty */
        /* fFilter[f]._similar = `(${fil.join('|')})`; */
      } else if (f === OPERATION_GROUP_NAME) {
        fFilter.push({
          operation: {
            operation_groups_operations: {
              operation_group_name: {
                _in: [...fil],
              },
            },
          },
        });
      } else if (f === SHOW_ONLY_ERRORS_SYMBOL) {
        fFilter.push({
          is_error: {
            _eq: true,
          },
        });
      } else if (f === TIME_RANGE_SYMBOL) {
        const timeFilters = [];
        const timeVal = appliedFilters[0];
        if (typeof timeVal.value === 'string') {
          timeFilters.push({
            time: {
              _gt: getTimeRangeValue(timeVal.value),
            },
          });
        } else {
          const fDate = new Date(timeVal.value.start);
          const toDate = new Date(timeVal.value.end);
          timeFilters.push({
            time: {
              _gt: fDate.toISOString(),
            },
          });
          timeFilters.push({
            time: {
              _lt: toDate.toISOString(),
            },
          });
        }
        fFilter.push(...timeFilters);
        // fFilter._and = [...timeFilters];
      } else if (f === CLIENT_NAME_SYMBOL) {
        fFilter.push({
          _or: [...fil],
        });
      } else if (f === HIDE_INTROPSECTION_QUERY_SYMBOL) {
        fFilter.push({
          operation_name: {
            _neq: 'IntrospectionQuery',
          },
        });
      } else {
        fFilter.push({
          [f]: {
            _in: [...fil],
          },
        });
      }
    }
  });
  return fFilter;
};

export {
  getTimeRangeValue,
  curried,
  curriedInputElement,
  curriedCheckboxElement,
  indexOf,
  getJson,
  capitalize,
  stripUnderScore,
  filterByType,
  transformedVals,
  transformExecutionTime,
  transformRequestResponseTime,
  transformedHeaderVal,
};
