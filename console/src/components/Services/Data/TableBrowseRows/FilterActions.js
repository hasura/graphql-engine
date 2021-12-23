// import Endpoints, {globalCookiePolicy} from '../../Endpoints';
import { defaultCurFilter } from '../DataState';
import { vMakeTableRequests, vMakeExportRequest } from './ViewActions';
import { Integers, Reals } from '../constants';
import {
  downloadObjectAsJsonFile,
  downloadObjectAsCsvFile,
  getCurrTimeForFileName,
  getConfirmation,
} from '../../../Common/utils/jsUtils';

const LOADING = 'ViewTable/FilterQuery/LOADING';

const SET_DEFQUERY = 'ViewTable/FilterQuery/SET_DEFQUERY';
const SET_FILTERCOL = 'ViewTable/FilterQuery/SET_FILTERCOL';
const SET_FILTEROP = 'ViewTable/FilterQuery/SET_FILTEROP';
const SET_FILTERVAL = 'ViewTable/FilterQuery/SET_FILTERVAL';
const ADD_FILTER = 'ViewTable/FilterQuery/ADD_FILTER';
const REMOVE_FILTER = 'ViewTable/FilterQuery/REMOVE_FILTER';

const SET_ORDERCOL = 'ViewTable/FilterQuery/SET_ORDERCOL';
const SET_ORDERTYPE = 'ViewTable/FilterQuery/SET_ORDERTYPE';
const ADD_ORDER = 'ViewTable/FilterQuery/ADD_ORDER';
const REMOVE_ORDER = 'ViewTable/FilterQuery/REMOVE_ORDER';
const SET_LIMIT = 'ViewTable/FilterQuery/SET_LIMIT';
const SET_OFFSET = 'ViewTable/FilterQuery/SET_OFFSET';
const SET_NEXTPAGE = 'ViewTable/FilterQuery/SET_NEXTPAGE';
const SET_PREVPAGE = 'ViewTable/FilterQuery/SET_PREVPAGE';
// const MAKING_REQUEST = 'ViewTable/FilterQuery/MAKING_REQUEST';
// const REQUEST_SUCCESS = 'ViewTable/FilterQuery/REQUEST_SUCCESS';
// const REQUEST_ERROR = 'ViewTable/FilterQuery/REQUEST_ERROR';

const setLoading = () => ({ type: LOADING, data: true });
const unsetLoading = () => ({ type: LOADING, data: false });
const setDefaultQuery = curQuery => ({ type: SET_DEFQUERY, curQuery });
const setFilterCol = (name, index) => ({ type: SET_FILTERCOL, name, index });
const setFilterOp = (opName, index) => ({ type: SET_FILTEROP, opName, index });
const setFilterVal = (val, index) => ({ type: SET_FILTERVAL, val, index });
const addFilter = () => ({ type: ADD_FILTER });
const removeFilter = index => ({ type: REMOVE_FILTER, index });

const setOrderCol = (name, index) => ({ type: SET_ORDERCOL, name, index });
const setOrderType = (order, index) => ({ type: SET_ORDERTYPE, order, index });
const addOrder = () => ({ type: ADD_ORDER });
const removeOrder = index => ({ type: REMOVE_ORDER, index });

const setLimit = limit => ({ type: SET_LIMIT, limit });
const setOffset = offset => ({ type: SET_OFFSET, offset });
const setNextPage = () => ({ type: SET_NEXTPAGE });
const setPrevPage = () => ({ type: SET_PREVPAGE });

const parseArray = val => {
  if (Array.isArray(val)) return val;
  try {
    return JSON.parse(val);
  } catch (err) {
    return '';
  }
};

const runQuery = tableSchema => {
  return (dispatch, getState) => {
    const state = getState().tables.view.curFilter;
    let finalWhereClauses = state.where.$and.filter(w => {
      const colName = Object.keys(w)[0].trim();
      if (colName === '') {
        return false;
      }
      const opName = Object.keys(w[colName])[0].trim();
      if (opName === '') {
        return false;
      }
      return true;
    });
    finalWhereClauses = finalWhereClauses.map(w => {
      const colName = Object.keys(w)[0];
      const opName = Object.keys(w[colName])[0];
      const val = w[colName][opName];

      if (['$in', '$nin'].includes(opName)) {
        w[colName][opName] = parseArray(val);
        return w;
      }

      const colType = tableSchema.columns.find(c => c.column_name === colName)
        .data_type;
      if (Integers.indexOf(colType) > 0) {
        w[colName][opName] = parseInt(val, 10);
        return w;
      }
      if (Reals.indexOf(colType) > 0) {
        w[colName][opName] = parseFloat(val);
        return w;
      }
      if (colType === 'boolean') {
        if (val === 'true') {
          w[colName][opName] = true;
        } else if (val === 'false') {
          w[colName][opName] = false;
        }
      }
      return w;
    });
    const newQuery = {
      where: { $and: finalWhereClauses },
      limit: state.limit,
      offset: state.offset,
      order_by: state.order_by.filter(w => w.column.trim() !== ''),
    };
    if (newQuery.where.$and.length === 0) {
      delete newQuery.where;
    }
    if (newQuery.order_by.length === 0) {
      delete newQuery.order_by;
    }
    dispatch({ type: 'ViewTable/V_SET_QUERY_OPTS', queryStuff: newQuery });
    dispatch(vMakeTableRequests());
  };
};

const exportDataQuery = (tableSchema, type) => {
  return (dispatch, getState) => {
    const count = getState().tables.view.count;

    const confirmed = getConfirmation(
      `There ${
        count === 1 ? 'is 1 row' : `are ${count} rows`
      } selected for export.`
    );
    if (!confirmed) {
      return;
    }

    const state = getState().tables.view.curFilter;
    let finalWhereClauses = state.where.$and.filter(w => {
      const colName = Object.keys(w)[0].trim();
      if (colName === '') {
        return false;
      }
      const opName = Object.keys(w[colName])[0].trim();
      if (opName === '') {
        return false;
      }
      return true;
    });

    finalWhereClauses = finalWhereClauses.map(w => {
      const colName = Object.keys(w)[0];
      const opName = Object.keys(w[colName])[0];
      const val = w[colName][opName];

      if (['$in', '$nin'].includes(opName)) {
        w[colName][opName] = parseArray(val);
        return w;
      }

      const colType = tableSchema.columns.find(c => c.column_name === colName)
        .data_type;
      if (Integers.indexOf(colType) > 0) {
        w[colName][opName] = parseInt(val, 10);
        return w;
      }
      if (Reals.indexOf(colType) > 0) {
        w[colName][opName] = parseFloat(val);
        return w;
      }
      if (colType === 'boolean') {
        if (val === 'true') {
          w[colName][opName] = true;
        } else if (val === 'false') {
          w[colName][opName] = false;
        }
      }
      return w;
    });
    const newQuery = {
      where: { $and: finalWhereClauses },
      order_by: state.order_by.filter(w => w.column.trim() !== ''),
    };
    if (newQuery.where.$and.length === 0) {
      delete newQuery.where;
    }
    if (newQuery.order_by.length === 0) {
      delete newQuery.order_by;
    }

    const { table_schema, table_name } = tableSchema;
    const fileName = `export_${table_schema}_${table_name}_${getCurrTimeForFileName()}`;

    dispatch({ type: 'ViewTable/V_SET_QUERY_OPTS', queryStuff: newQuery });
    dispatch(vMakeExportRequest()).then(d => {
      if (d) {
        if (type === 'JSON') downloadObjectAsJsonFile(fileName, d);
        else if (type === 'CSV') downloadObjectAsCsvFile(fileName, d);
      }
    });
  };
};
const filterReducer = (state = defaultCurFilter, action) => {
  const i = action.index;
  const newFilter = {};
  switch (action.type) {
    case SET_DEFQUERY:
      const q = action.curQuery;
      if (
        'order_by' in q ||
        'limit' in q ||
        'offset' in q ||
        ('where' in q && '$and' in q.where)
      ) {
        const newCurFilterQ = {};
        newCurFilterQ.where =
          'where' in q && '$and' in q.where
            ? { $and: [...q.where.$and, { '': { $eq: '' } }] }
            : { ...defaultCurFilter.where };
        newCurFilterQ.order_by =
          'order_by' in q
            ? [...q.order_by, ...defaultCurFilter.order_by]
            : [...defaultCurFilter.order_by];
        newCurFilterQ.limit = q.limit || defaultCurFilter.limit;
        newCurFilterQ.offset = q.offset || defaultCurFilter.offset;
        return newCurFilterQ;
      }
      return defaultCurFilter;
    case SET_FILTERCOL:
      const oldColName = Object.keys(state.where.$and[i])[0];
      newFilter[action.name] = { ...state.where.$and[i][oldColName] };
      return {
        ...state,
        where: {
          $and: [
            ...state.where.$and.slice(0, i),
            newFilter,
            ...state.where.$and.slice(i + 1),
          ],
        },
      };
    case SET_FILTEROP:
      const colName = Object.keys(state.where.$and[i])[0];
      const oldOp = Object.keys(state.where.$and[i][colName])[0];
      newFilter[colName] = {};
      newFilter[colName][action.opName] = state.where.$and[i][colName][oldOp];
      return {
        ...state,
        where: {
          $and: [
            ...state.where.$and.slice(0, i),
            newFilter,
            ...state.where.$and.slice(i + 1),
          ],
        },
      };
    case SET_FILTERVAL:
      const colName1 = Object.keys(state.where.$and[i])[0];
      const opName = Object.keys(state.where.$and[i][colName1])[0];
      newFilter[colName1] = {};
      newFilter[colName1][opName] = action.val;
      return {
        ...state,
        where: {
          $and: [
            ...state.where.$and.slice(0, i),
            newFilter,
            ...state.where.$and.slice(i + 1),
          ],
        },
      };
    case ADD_FILTER:
      return {
        ...state,
        where: {
          $and: [...state.where.$and, { '': { $eq: '' } }],
        },
      };
    case REMOVE_FILTER:
      const newFilters = [
        ...state.where.$and.slice(0, i),
        ...state.where.$and.slice(i + 1),
      ];
      return {
        ...state,
        where: { $and: newFilters },
      };

    case SET_ORDERCOL:
      const oldOrder = state.order_by[i];
      return {
        ...state,
        order_by: [
          ...state.order_by.slice(0, i),
          { ...oldOrder, column: action.name },
          ...state.order_by.slice(i + 1),
        ],
      };
    case SET_ORDERTYPE:
      const oldOrder1 = state.order_by[i];
      return {
        ...state,
        order_by: [
          ...state.order_by.slice(0, i),
          { ...oldOrder1, type: action.order },
          ...state.order_by.slice(i + 1),
        ],
      };
    case REMOVE_ORDER:
      return {
        ...state,
        order_by: [
          ...state.order_by.slice(0, i),
          ...state.order_by.slice(i + 1),
        ],
      };
    case ADD_ORDER:
      return {
        ...state,
        order_by: [
          ...state.order_by,
          { column: '', type: 'asc', nulls: 'last' },
        ],
      };

    case SET_LIMIT:
      return {
        ...state,
        limit: action.limit,
      };
    case SET_OFFSET:
      return {
        ...state,
        offset: action.offset,
      };
    case SET_NEXTPAGE:
      return {
        ...state,
        offset: state.offset + state.limit,
      };
    case SET_PREVPAGE:
      const newOffset = state.offset - state.limit;
      return {
        ...state,
        offset: newOffset < 0 ? 0 : newOffset,
      };
    case LOADING:
      return {
        ...state,
        loading: action.data,
      };
    default:
      return state;
  }
};

export default filterReducer;
export {
  setFilterCol,
  setFilterOp,
  setFilterVal,
  addFilter,
  removeFilter,
  setOrderCol,
  setOrderType,
  addOrder,
  removeOrder,
  setLimit,
  setOffset,
  setNextPage,
  setPrevPage,
  setDefaultQuery,
  setLoading,
  unsetLoading,
  runQuery,
  exportDataQuery,
};
