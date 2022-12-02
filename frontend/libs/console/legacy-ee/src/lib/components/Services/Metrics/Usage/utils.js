/* Checks whether a condition matches on a list of elements, if yes returns the index else -1 */
import moment from 'moment';
import { aliasedNames, actualTypeToAlias } from './constants';

import {
  TIME_RANGE_BY_HOUR,
  TIME_RANGE_BY_6_HOURS,
  TIME_RANGE_BY_12_HOURS,
  TIME_RANGE_BY_DAY,
} from '../constants';

export const getIfAliased = c => {
  if (c in aliasedNames) {
    return aliasedNames[c];
  }
  return c;
};

export const getActualIfAliased = c => {
  if (c in actualTypeToAlias) {
    return actualTypeToAlias[c];
  }
  return c;
};

const transformTimeType = d => {
  return moment(d).fromNow();
};

const transformExecutionTime = d => {
  return Math.round(d * 1000 * 100) / 100;
};

const transformResponseTime = d => {
  return Math.round((d / 1000) * 100) / 100;
};

const transformToTwoPlaces = d => {
  return Math.round(d * 100) / 100;
};

const transformedVals = {
  time: transformTimeType,
  average_execution_time: transformExecutionTime,
  average_response_size: transformResponseTime,
};

const transformExecutionTimeHeader = val => {
  return `${val} ms`;
};
const transformResponseSizeHeader = val => {
  return `${val} kB`;
};

const transformedHeaderVal = {
  average_execution_time: transformExecutionTimeHeader,
  average_response_size: transformResponseSizeHeader,
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

const getTimeInterval = symbol => {
  switch (symbol) {
    case TIME_RANGE_BY_HOUR:
      return '5 minutes';
    case TIME_RANGE_BY_DAY:
      return '1 hour';
    case TIME_RANGE_BY_6_HOURS:
      return '30 minutes';
    case TIME_RANGE_BY_12_HOURS:
      return '1 hour';
    default:
      return '1 day';
  }
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
  transformResponseTime,
  transformedHeaderVal,
  getTimeInterval,
  transformToTwoPlaces,
};
