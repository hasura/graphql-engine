/*
Use state exactly the way columns in create table do.
dispatch actions using a given function,
but don't listen to state.
derive everything through viewtable as much as possible.
*/
import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { createHistory } from 'history';

import {
  setFilterCol,
  setFilterOp,
  setFilterVal,
  addFilter,
  removeFilter,
} from './FilterActions.js';
import {
  setOrderCol,
  setOrderType,
  addOrder,
  removeOrder,
} from './FilterActions.js';
import {
  setDefaultQuery,
  runQuery,
  exportDataQuery,
  setOffset,
} from './FilterActions';
import Button from '../../../Common/Button/Button';
import ReloadEnumValuesButton from '../Common/Components/ReloadEnumValuesButton';
import styles from '../../../Common/FilterQuery/FilterQuery.scss';
import { getPersistedPageSize } from './tableUtils';
import { isEmpty } from '../../../Common/utils/jsUtils';
import ExportData from './ExportData';
import { dataSource } from '../../../../dataSources';

const history = createHistory();

const renderCols = (
  colName,
  tableSchema,
  onChange,
  usage,
  key,
  skipColumns
) => {
  let columns = tableSchema.columns.map(c => c.column_name);
  if (skipColumns) {
    columns = columns.filter(n => !skipColumns.includes(n) || n === colName);
  }

  return (
    <select
      className="form-control"
      onChange={onChange}
      value={colName.trim()}
      data-test={
        usage === 'sort' ? `sort-column-${key}` : `filter-column-${key}`
      }
    >
      {colName.trim() === '' ? (
        <option disabled value="">
          -- column --
        </option>
      ) : null}
      {columns.map((c, i) => (
        <option key={i} value={c}>
          {c}
        </option>
      ))}
    </select>
  );
};

const renderOps = (opName, onChange, key) => (
  <select
    className="form-control"
    onChange={onChange}
    value={opName.trim()}
    data-test={`filter-op-${key}`}
  >
    {opName.trim() === '' ? (
      <option disabled value="">
        -- op --
      </option>
    ) : null}
    {dataSource.operators.map((o, i) => (
      <option key={i} value={o.value}>
        {`[${o.graphqlOp}] ${o.name}`}
      </option>
    ))}
  </select>
);

const getDefaultValue = (possibleValue, opName) => {
  if (possibleValue) {
    if (Array.isArray(possibleValue)) return JSON.stringify(possibleValue);
    return possibleValue;
  }

  const operator = dataSource.operators.find(op => op.value === opName);
  return operator && operator.defaultValue ? operator.defaultValue : '';
};

const renderWheres = (whereAnd, tableSchema, dispatch) => {
  return whereAnd.map((clause, i) => {
    const colName = Object.keys(clause)[0];
    const opName = Object.keys(clause[colName])[0];
    const dSetFilterCol = e => {
      dispatch(setFilterCol(e.target.value, i));
    };
    const dSetFilterOp = e => {
      dispatch(setFilterOp(e.target.value, i));
    };
    let removeIcon = null;
    if (i + 1 < whereAnd.length) {
      removeIcon = (
        <i
          className="fa fa-times"
          onClick={() => {
            dispatch(removeFilter(i));
          }}
          data-test={`clear-filter-${i}`}
        />
      );
    }

    return (
      <div key={i} className={`${styles.inputRow} row`}>
        <div className="col-xs-4">
          {renderCols(colName, tableSchema, dSetFilterCol, 'filter', i, [])}
        </div>
        <div className="col-xs-3">{renderOps(opName, dSetFilterOp, i)}</div>
        <div className="col-xs-4">
          <input
            className="form-control"
            placeholder="-- value --"
            value={getDefaultValue(clause[colName][opName], opName)}
            onChange={e => {
              dispatch(setFilterVal(e.target.value, i));
              if (i + 1 === whereAnd.length) {
                dispatch(addFilter());
              }
            }}
            data-test={`filter-value-${i}`}
          />
        </div>
        <div className="text-center col-xs-1">{removeIcon}</div>
      </div>
    );
  });
};

const renderSorts = (orderBy, tableSchema, dispatch) => {
  const currentOrderBy = orderBy.map(o => o.column);
  return orderBy.map((c, i) => {
    const dSetOrderCol = e => {
      dispatch(setOrderCol(e.target.value, i));
      if (i + 1 === orderBy.length) {
        dispatch(addOrder());
      }
    };
    let removeIcon = null;
    if (i + 1 < orderBy.length) {
      removeIcon = (
        <i
          className="fa fa-times"
          onClick={() => {
            dispatch(removeOrder(i));
          }}
          data-test={`clear-sorts-${i}`}
        />
      );
    }

    return (
      <div key={i} className={`${styles.inputRow} row`}>
        <div className="col-xs-6">
          {renderCols(
            c.column,
            tableSchema,
            dSetOrderCol,
            'sort',
            i,
            currentOrderBy
          )}
        </div>
        <div className="col-xs-5">
          <select
            value={c.column ? c.type : ''}
            className="form-control"
            onChange={e => {
              dispatch(setOrderType(e.target.value, i));
            }}
            data-test={`sort-order-${i}`}
          >
            <option disabled value="">
              --
            </option>
            <option value="asc">Asc</option>
            <option value="desc">Desc</option>
          </select>
        </div>
        <div className="col-xs-1 text-center">{removeIcon}</div>
      </div>
    );
  });
};

class FilterQuery extends Component {
  componentDidMount() {
    const { dispatch, tableSchema, curQuery } = this.props;
    const limit = getPersistedPageSize();
    if (isEmpty(this.props.urlQuery)) {
      dispatch(setDefaultQuery({ ...curQuery, limit }));
      return;
    }

    let urlFilters = [];
    if (typeof this.props.urlQuery.filter === 'string') {
      urlFilters = [this.props.urlQuery.filter];
    } else if (Array.isArray(this.props.urlQuery.filter)) {
      urlFilters = this.props.urlQuery.filter;
    }
    const where = {
      $and: urlFilters.map(filter => {
        const parts = filter.split(';');
        const col = parts[0];
        const op = parts[1];
        const value = parts[2];
        return { [col]: { [op]: value } };
      }),
    };

    let urlSorts = [];
    if (typeof this.props.urlQuery.sort === 'string') {
      urlSorts = [this.props.urlQuery.sort];
    } else if (Array.isArray(this.props.urlQuery.sort)) {
      urlSorts = this.props.urlQuery.sort;
    }

    const order_by = urlSorts.map(sort => {
      const parts = sort.split(';');
      const column = parts[0];
      const type = parts[1];
      const nulls = 'last';
      return { column, type, nulls };
    });

    dispatch(setDefaultQuery({ where, order_by, limit }));
    dispatch(runQuery(tableSchema));
  }

  setParams(query = { filters: [], sorts: [] }) {
    const searchParams = new URLSearchParams();
    query.filters.forEach(filter => searchParams.append('filter', filter));
    query.sorts.forEach(sort => searchParams.append('sort', sort));
    return searchParams.toString();
  }

  setUrlParams(whereAnd, orderBy) {
    const sorts = orderBy
      .filter(order => order.column)
      .map(order => `${order.column};${order.type}`);
    const filters = whereAnd
      .filter(
        where => Object.keys(where).length === 1 && Object.keys(where)[0] !== ''
      )
      .map(where => {
        const col = Object.keys(where)[0];
        const op = Object.keys(where[col])[0];
        const value = where[col][op];
        return `${col};${op};${value}`;
      });
    const url = this.setParams({ filters, sorts });
    history.push({
      pathname: history.getCurrentLocation().pathname,
      search: `?${url}`,
    });
  }

  render() {
    const { dispatch, whereAnd, tableSchema, orderBy } = this.props; // eslint-disable-line no-unused-vars
    const exportData = type => {
      dispatch(exportDataQuery(tableSchema, type));
    };

    return (
      <div className={styles.add_mar_top}>
        <form
          onSubmit={e => {
            e.preventDefault();
            dispatch(setOffset(0));
            this.setUrlParams(whereAnd, orderBy);
            dispatch(runQuery(tableSchema));
          }}
        >
          <div>
            <div
              className={`${styles.queryBox} col-xs-6 ${styles.padd_left_remove}`}
            >
              <span className={styles.subheading_text}>Filter</span>
              {renderWheres(whereAnd, tableSchema, dispatch)}
            </div>
            <div
              className={`${styles.queryBox} col-xs-6 ${styles.padd_left_remove}`}
            >
              <b className={styles.subheading_text}>Sort</b>
              {renderSorts(orderBy, tableSchema, dispatch)}
            </div>
          </div>
          <div className={`${styles.padd_right} ${styles.clear_fix}`}>
            <Button
              type="submit"
              color="yellow"
              size="sm"
              data-test="run-query"
              className={styles.add_mar_right}
            >
              Run query
            </Button>
            <ExportData onExport={exportData} />
            {tableSchema.is_enum ? (
              <ReloadEnumValuesButton
                dispatch={dispatch}
                tooltipStyle={styles.add_mar_left_mid}
              />
            ) : null}
          </div>
        </form>
      </div>
    );
  }
}

FilterQuery.propTypes = {
  curQuery: PropTypes.object.isRequired,
  tableSchema: PropTypes.object.isRequired,
  whereAnd: PropTypes.array.isRequired,
  orderBy: PropTypes.array.isRequired,
  limit: PropTypes.number.isRequired,
  count: PropTypes.number,
  tableName: PropTypes.string,
  offset: PropTypes.number.isRequired,
  dispatch: PropTypes.func.isRequired,
};

export default FilterQuery;
