/*
  Use state exactly the way columns in create table do.
  dispatch actions using a given function,
  but don't listen to state.
  derive everything through viewtable as much as possible.
*/
import PropTypes from 'prop-types';

import React, { Component } from 'react';
import { Operators } from '../constants';
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
import { setDefaultQuery, runQuery, setOffset } from './FilterActions';
import Button from '../../../Common/Button/Button';

const renderCols = (colName, tableSchema, onChange, usage, key) => {
  const columns = tableSchema.columns.map(c => c.column_name);
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
    {Operators.map((o, i) => (
      <option key={i} value={o.value}>
        {o.value}
      </option>
    ))}
  </select>
);

const renderWheres = (whereAnd, tableSchema, dispatch) => {
  const styles = require('../../../Common/FilterQuery/FilterQuery.scss');
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
          {renderCols(colName, tableSchema, dSetFilterCol, 'filter', i)}
        </div>
        <div className="col-xs-3">{renderOps(opName, dSetFilterOp, i)}</div>
        <div className="col-xs-4">
          <input
            className="form-control"
            placeholder="-- value --"
            value={clause[colName][opName]}
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
  const styles = require('../../../Common/FilterQuery/FilterQuery.scss');
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
          {renderCols(c.column, tableSchema, dSetOrderCol, 'sort', i)}
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
    const dispatch = this.props.dispatch;
    dispatch(setDefaultQuery(this.props.curQuery));
  }

  render() {
    const { dispatch, whereAnd, tableSchema, orderBy } = this.props; // eslint-disable-line no-unused-vars
    const styles = require('../../../Common/FilterQuery/FilterQuery.scss');
    return (
      <div className={styles.filterOptions}>
        <form
          onSubmit={e => {
            e.preventDefault();
            dispatch(setOffset(0));
            dispatch(runQuery(tableSchema));
          }}
        >
          <div className="">
            <div
              className={`${styles.queryBox} col-xs-6 ${
                styles.padd_left_remove
              }`}
            >
              <span className={styles.subheading_text}>Filter</span>
              {renderWheres(whereAnd, tableSchema, dispatch)}
            </div>
            <div
              className={`${styles.queryBox} col-xs-6 ${
                styles.padd_left_remove
              }`}
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
            >
              Run query
            </Button>
            {/* <div className={styles.count + ' alert alert-info'}><i>Total <b>{tableName}</b> rows in the database for current query: {count} </i></div> */}
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
