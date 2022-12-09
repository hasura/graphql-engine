import moment from 'moment';
import queryString from 'query-string';

import Endpoints from '../../../Endpoints';
import {
  CLIENT_NAME_SYMBOL,
  ERROR_CODE_SYMBOL,
  NO_TITLE_MAP,
  OPERATION_GROUP_NAME,
  OPERATION_TYPE_SYMBOL,
  relativeModulePath,
  ROLE_SYMBOL,
  SUBSCRIPTION_STATUS_SYMBOL,
  TIME_RANGE_BY_HOUR,
  TIME_RANGE_SYMBOL,
  timeRangeFilters,
  TRANSPORT_SYMBOL,
  transportFilters,
  WEBSOCKET_STATUS_SYMBOL,
} from './constants';

export const parseQueryString = search => {
  return queryString.parse(search);
};
const updateQsHistory = (qs = window.encodeURI('?filters=[]')) => {
  window.history.pushState('', '', qs);
};

const updateQsHistorywithGroupbys = (
  qs = window.encodeURI('?filters=[]&groupbys=[]')
) => {
  window.history.pushState('', '', qs);
};

const transformToTypeValueArr = values => {
  const vals = Object.keys(values);
  return vals.map(v => {
    return {
      type: values[v],
      value: v,
    };
  });
};

const arrayToPostgresArray = values =>
  '{' + (values ? values.join(', ') : '') + '}';

const urlToPageHeaderMap = {
  [`${relativeModulePath}`]: 'Overview',
  [`${relativeModulePath}/error`]: 'Errors',
  [`${relativeModulePath}/system-health`]: 'System-Health',
  [`${relativeModulePath}/operations`]: 'Operations',
  [`${relativeModulePath}/allow-lists`]: 'Allow Lists',
  [`${relativeModulePath}/usage`]: 'Usage',
  [`${relativeModulePath}/regression-tests`]: 'Regression Tests',
  [`${relativeModulePath}/api-limits`]: 'API Limits',
  [`${relativeModulePath}/websockets`]: 'Websockets',
  [`${relativeModulePath}/subscription-workers`]: 'Subscription Workers',
};

const DEFAULT_FILTERS = [{ type: 'time_range', value: TIME_RANGE_BY_HOUR }];

const applyFilterByQueryParam = (
  search,
  applyDefaultFilters = false,
  defaultGroupBys
) => {
  const parsedQS = parseQueryString(search);
  if ('filters' in parsedQS || 'groupbys' in parsedQS) {
    let parsedFilter = [];
    let parsedGroupbys = [];
    try {
      if (parsedQS.filters) {
        parsedFilter = JSON.parse(parsedQS.filters);
      }
      if (parsedQS.groupbys) {
        parsedGroupbys = JSON.parse(parsedQS.groupbys);
      }
      if (parsedFilter.length > 0 || parsedGroupbys.length > 0) {
        return {
          parsedFilter: parsedFilter || DEFAULT_FILTERS,
          parsedGroupbys,
        };
      }
      return {
        parsedFilter: DEFAULT_FILTERS,
      };
    } catch (e) {
      // reduxDispatch(_replace({ pathname }));
      console.error(e);
      updateQsHistory();
      return {
        parsedFilter: DEFAULT_FILTERS,
      };
    }
  }
  if (applyDefaultFilters) {
    let qs = `?filters=${window.encodeURI(JSON.stringify(DEFAULT_FILTERS))}`;
    if (defaultGroupBys) {
      qs = `?filters=${window.encodeURI(
        JSON.stringify(DEFAULT_FILTERS)
      )}&groupbys=${window.encodeURI(JSON.stringify(defaultGroupBys))}`;
    }
    updateQsHistory(qs);
    return { parsedFilter: DEFAULT_FILTERS, parsedGroupbys: defaultGroupBys };
  }
  return {};
};

const retrieveDefaultDropdownOptions = value => {
  switch (value) {
    case TIME_RANGE_SYMBOL:
      return timeRangeFilters;
    case TRANSPORT_SYMBOL:
      return transportFilters;
    default:
      console.error('Unknown type for default dropdown options');
  }
};

const retrieveFilterData = (data, filter) => {
  let nodeType = '';
  switch (filter.value) {
    case OPERATION_TYPE_SYMBOL:
      nodeType = 'operation_types';
      break;
    case CLIENT_NAME_SYMBOL:
      nodeType = 'client_names';
      break;
    case ROLE_SYMBOL:
      nodeType = 'user_roles';
      break;
    case ERROR_CODE_SYMBOL:
      nodeType = 'error_codes';
      break;
    case OPERATION_GROUP_NAME:
      nodeType = 'operation_groups';
      break;
    case TRANSPORT_SYMBOL:
      nodeType = 'transports';
      break;
    case SUBSCRIPTION_STATUS_SYMBOL:
      nodeType = 'subscriptionWorkerStatus';
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

const getValuesForEmptyMap = () => {
  return Object.keys(NO_TITLE_MAP).map(m => NO_TITLE_MAP[m]);
};

const onFilterChangeCb = (nextFilters, groupBys) => {
  if (nextFilters.length > 0 || groupBys.length > 0) {
    const qs = `?filters=${window.encodeURI(
      JSON.stringify(nextFilters)
    )}&groupbys=${window.encodeURI(JSON.stringify(groupBys))}`;
    updateQsHistory(qs);
  } else {
    updateQsHistorywithGroupbys();
  }
};

const onGroupByChangeCb = (nextGroupBys, filters) => {
  if (nextGroupBys.length > 0 || filters.length > 0) {
    const qs = `?filters=${window.encodeURI(
      JSON.stringify(filters)
    )}&groupbys=${window.encodeURI(JSON.stringify(nextGroupBys))}`;
    updateQsHistory(qs);
  } else {
    updateQsHistorywithGroupbys();
  }
};

const getProjectId = state => {
  if (state.main.projectId) {
    return state.main.projectId;
  }
  if (window?.__env?.projectId) {
    return window?.__env?.projectId;
  }
  return null;
};

const getProjectInfo = state => {
  if (state.main.projectId) {
    return {
      id: state.main.projectId,
      name: state.main.projectName,
    };
  }
  return null;
};

// hide variable shape, for example: { key: '<..>' }
const hideVariableValues = variables => {
  if (variables === null || variables === undefined) {
    return '<..>';
  }

  switch (typeof variables) {
    case 'object':
      if (Array.isArray(variables)) {
        return variables.map(hideVariableValues);
      }

      return Object.keys(variables).reduce(
        (acc, key) => ({
          ...acc,
          [key]: hideVariableValues(variables[key]),
        }),
        {}
      );
    default:
      return '<..>';
  }
};

const getTimeIntervalFromRange = (start, end, len = 6) => {
  let duration = moment(end).diff(start, 'minutes');

  if (duration <= 60) {
    return '5m';
  }

  if (duration <= 120) {
    return '10m';
  }

  duration = duration / 60;
  if (duration <= 24) {
    return '1h';
  }

  duration = duration / 24;
  if (duration <= 30) {
    return '1 day';
  }

  duration = duration / 30;
  if (duration <= 12) {
    return `${Math.ceil(duration / len)} months`;
  }

  return `${Math.ceil(duration / len / 12)} years`;
};

const getMetricsUrl = metricsFQDN => {
  if (metricsFQDN && metricsFQDN.length > 0) {
    return `https://${metricsFQDN}/v1/graphql`;
  }
  return Endpoints.metricsGraphQLUrl;
};
/**
 *
 *
 * @param {['admin'|'graphql_admin'|'view_metrics']} privileges
 * @return {boolean}
 */
const isAdmin = privileges => (privileges || []).some(p => p === 'admin');

/**
 * Utility to parse http and non HTTP URLs
 * @param  url : URL string/ postgres connection URL
 * @returns {
 *    protocol: string
 *    user: string
 *    password: string
 *    host: string
 *    hostname: string
 *    port: string
 *    segments: string
 *    params:  object,
 *  };
 */
const parseURI = url => {
  try {
    const protocol = new URL(url).protocol;
    const newUrl = url.replace(protocol, 'http://');
    const parsed = new URL(newUrl);
    return {
      origin: parsed.origin.replace('http:', protocol),
      hash: parsed.hash,
      host: parsed.host,
      hostname: parsed.hostname,
      port: parsed.port,
      href: parsed.href.replace('http:', protocol),
      password: parsed.password,
      pathname: parsed.pathname,
      search: parsed.search,
      username: parsed.username,
      protocol,
    };
  } catch (error) {
    return {};
  }
};
export {
  applyFilterByQueryParam,
  arrayToPostgresArray,
  getMetricsUrl,
  getProjectId,
  getProjectInfo,
  getTimeIntervalFromRange,
  getValuesForEmptyMap,
  hideVariableValues,
  isAdmin,
  onFilterChangeCb,
  onGroupByChangeCb,
  retrieveDefaultDropdownOptions,
  retrieveFilterData,
  transformToTypeValueArr,
  updateQsHistory,
  urlToPageHeaderMap,
  parseURI,
};
