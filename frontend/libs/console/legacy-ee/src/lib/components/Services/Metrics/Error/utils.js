/* Checks whether a condition matches on a list of elements, if yes returns the index else -1 */
import { aliasedColumns, actualTypeToAlias } from './constants';

import {
  TIME_RANGE_BY_HOUR,
  TIME_RANGE_BY_6_HOURS,
  TIME_RANGE_BY_12_HOURS,
  TIME_RANGE_BY_DAY,
  TIME_RANGE_SYMBOL,
  EMPTY_OPERATION_TYPE_SYMBOL,
  EMPTY_ROLE_SYMBOL,
  EMPTY_ERROR_CODE_SYMBOL,
  EMPTY_OPERATION_NAME_SYMBOL,
  EMPTY_CLIENT_NAME_SYMBOL,
  OPERATION_NAME_SYMBOL,
  OPERATION_TYPE_SYMBOL,
  ROLE_SYMBOL,
  ERROR_CODE_SYMBOL,
} from '../constants';
import { getValuesForEmptyMap } from '../utils';

export const createFilter = (type, value) => {
  return {
    type: type,
    value: value,
  };
};

export const checkIfEmptyReturnAppropriateValue = n => {
  if (n === ERROR_CODE_SYMBOL) {
    return EMPTY_ERROR_CODE_SYMBOL;
  }
  if (n === ROLE_SYMBOL) {
    return EMPTY_ROLE_SYMBOL;
  }
  if (n === OPERATION_NAME_SYMBOL) {
    return EMPTY_OPERATION_NAME_SYMBOL;
  }
  if (n === OPERATION_TYPE_SYMBOL) {
    return EMPTY_OPERATION_TYPE_SYMBOL;
  }
  return '';
};

export const getFilterNameFromEmptyValue = n => {
  if (n === EMPTY_ERROR_CODE_SYMBOL) {
    return ERROR_CODE_SYMBOL;
  }
  if (n === EMPTY_ROLE_SYMBOL) {
    return ROLE_SYMBOL;
  }
  if (n === EMPTY_OPERATION_NAME_SYMBOL) {
    return OPERATION_NAME_SYMBOL;
  }
  if (n === EMPTY_OPERATION_TYPE_SYMBOL) {
    return OPERATION_TYPE_SYMBOL;
  }
  return '';
};

export const getIfAliased = c => {
  if (c in aliasedColumns) {
    return aliasedColumns[c];
  }
  return c;
};

export const getActualIfAliased = c => {
  if (c in actualTypeToAlias) {
    return actualTypeToAlias[c];
  }
  return c;
};

const parseGroupByArrayType = q => {
  const regex = /{(.*)}/m;
  return regex.exec(q);
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
  list.forEach(l => {
    if (typeof l[key] === 'string') {
      r[l[key]] = true;
    } else {
      r.Custom = true;
    }
  });
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

const detectAdjective = n => {
  if (n > 1) return 's';
  return '';
};

const getSelectedValue = (selectedValues, defaultValue) => {
  let filteredElement = defaultValue;
  const totalSelectedValues = Object.keys(selectedValues);
  if (totalSelectedValues.length > 0) {
    filteredElement = totalSelectedValues[0];
  }
  return filteredElement;
};

const getSelectedFiltersCount = selectedValues => {
  let filteredElement = 'No filter applied';

  const totalSelectedValues = Object.keys(selectedValues);
  if (totalSelectedValues.length > 0) {
    filteredElement = `${totalSelectedValues.length} filter${detectAdjective(
      totalSelectedValues.length
    )} applied`;
  }
  return filteredElement;
};

export const getWhereClauseEx = (filterMap, filters, arraySerializer) => {
  const fFilter = {};
  const emptyValues = getValuesForEmptyMap();
  Object.keys(filterMap).forEach(f => {
    if (f !== TIME_RANGE_SYMBOL) {
      const appliedFilters = filterByType(filters, f);
      const fil = [];
      appliedFilters.forEach(e => {
        if (emptyValues.indexOf(e.value) !== -1) {
          fil.push('""');
          if (e.value === EMPTY_CLIENT_NAME_SYMBOL) {
            fil.push('null');
          }
        } else {
          fil.push(e.value);
        }
      });
      fFilter[f] =
        arraySerializer && typeof arraySerializer === 'function'
          ? arraySerializer(fil)
          : fil;
    }
  });
  return fFilter;
};

export const getWhereClause = (filterMap, filters) => {
  return getWhereClauseEx(filterMap, filters, arr => `{${arr.join(',')}}`);
};

export const getSelectedLength = o => {
  return Object.keys(o).length;
};

export {
  getSelectedFiltersCount,
  getSelectedValue,
  detectAdjective,
  getTimeRangeValue,
  curried,
  indexOf,
  getJson,
  capitalize,
  stripUnderScore,
  filterByType,
  parseGroupByArrayType,
  removeSecondsMiliseconds,
};
