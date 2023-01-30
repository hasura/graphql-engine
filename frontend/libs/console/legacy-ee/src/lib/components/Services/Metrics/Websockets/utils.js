/* Checks whether a condition matches on a list of elements, if yes returns the index else -1 */
import {
  aliasedColumns,
  actualTypeToAlias,
  NO_TITLE_MAP,
  timeRangeFilters,
} from './constants';

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
  CLIENT_NAME_SYMBOL,
  ROLE_SYMBOL,
  ERROR_CODE_SYMBOL,
  OPERATION_GROUP_NAME,
  WEBSOCKET_ID_SYMBOL,
  WEBSOCKET_STATUS_SYMBOL,
} from '../constants';

import moment from 'moment';

export const retrieveDefaultDropdownOptions = value => {
  switch (value) {
    case TIME_RANGE_SYMBOL:
      return timeRangeFilters;
    default:
      console.error('Unknown type for default dropdown options');
  }
};

export const retrieveFilterData = (data, filter) => {
  let nodeType = '';
  switch (filter.value) {
    case CLIENT_NAME_SYMBOL:
      nodeType = 'client_names';
      break;
    case ROLE_SYMBOL:
      nodeType = 'user_roles';
      break;
    case WEBSOCKET_ID_SYMBOL:
      nodeType = 'websocket_id';
      break;
    case OPERATION_GROUP_NAME:
      nodeType = 'operation_groups';
      break;
    case WEBSOCKET_STATUS_SYMBOL:
      nodeType = 'websocketStatus';
      break;
    default:
      console.error('Type not found');
  }
  if (nodeType.length === 0) return [];
  return data[nodeType];
};

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

export const getValuesForEmptyMap = () => {
  return Object.keys(NO_TITLE_MAP).map(m => NO_TITLE_MAP[m]);
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
        if (f === ROLE_SYMBOL) {
          fil.push({
            user_role: {
              _eq: e.value,
            },
          });
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
      if (f === TIME_RANGE_SYMBOL) {
        const timeFilters = [];
        const timeVal = appliedFilters[0];
        if (typeof timeVal.value === 'string') {
          timeFilters.push({
            start_time: {
              _gt: getTimeRangeValue(timeVal.value),
            },
          });
        } else {
          const fDate = new Date(timeVal.value.start);
          const toDate = new Date(timeVal.value.end);
          timeFilters.push({
            start_time: {
              _gt: fDate.toISOString(),
            },
          });
          timeFilters.push({
            start_time: {
              _gt: toDate.toISOString(),
            },
          });
        }
        fFilter.push(...timeFilters);
        // fFilter._and = [...timeFilters];
      } else if (f === CLIENT_NAME_SYMBOL) {
        fFilter.push({
          _or: [...fil],
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

export const getSelectedLength = o => {
  return Object.keys(o).length;
};

export const getColWidth = (header, contentRows = []) => {
  const MAX_WIDTH = 600;
  const HEADER_PADDING = 62;
  const CONTENT_PADDING = 36;
  const HEADER_FONT = 'bold 16px Gudea';
  const CONTENT_FONT = '14px Gudea';

  const getTextWidth = (text, font) => {
    // Doesn't work well with non-monospace fonts
    // const CHAR_WIDTH = 8;
    // return text.length * CHAR_WIDTH;

    // if given, use cached canvas for better performance
    // else, create new canvas
    const canvas =
      getTextWidth.canvas ||
      (getTextWidth.canvas = document.createElement('canvas'));

    const context = canvas.getContext('2d');
    context.font = font;

    const metrics = context.measureText(text);
    return metrics.width;
  };

  let maxContentWidth = 0;
  for (let i = 0; i < contentRows.length; i++) {
    if (contentRows[i] !== undefined && contentRows[i][header] !== null) {
      const content = contentRows[i][header];

      let contentString;
      if (header === 'time') {
        contentString = moment(content).fromNow();
      } else if (content === null || content === undefined) {
        contentString = 'NULL';
      } else if (typeof content === 'object') {
        contentString = JSON.stringify(content, null, 4);
      } else {
        contentString = content.toString();
      }

      const currLength = getTextWidth(contentString, CONTENT_FONT);

      if (currLength > maxContentWidth) {
        maxContentWidth = currLength;
      }
    }
  }

  const maxContentCellWidth = maxContentWidth + CONTENT_PADDING + 12;

  const headerCellWidth =
    getTextWidth(getIfAliased(header), HEADER_FONT) + HEADER_PADDING;

  return Math.min(MAX_WIDTH, Math.max(maxContentCellWidth, headerCellWidth));
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
