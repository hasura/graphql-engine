/*
  Use state exactly the way columns in create table do.
  dispatch actions using a given function,
  but don't listen to state.
  derive everything through viewtable as much as possible.
*/
import PropTypes from 'prop-types';

import React, { Component } from 'react';
import Operators from '../Operators';
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
import Button from '../../../Common/Button/Button';
import { setDefaultQuery, runQuery } from './FilterActions';
import { vMakeRequest } from './ViewActions';

const renderCols = (colName, triggerSchema, onChange, usage, key) => {
  const columns = ['id', 'delivered', 'created_at'];
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

const renderWheres = (whereAnd, triggerSchema, dispatch) => {
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
          {renderCols(colName, triggerSchema, dSetFilterCol, 'filter', i)}
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

const renderSorts = (orderBy, triggerSchema, dispatch) => {
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
          {renderCols(c.column, triggerSchema, dSetOrderCol, 'sort', i)}
        </div>
        <div className="col-xs-5">
          <select
            value={c.type}
            className="form-control"
            onChange={e => {
              dispatch(setOrderType(e.target.value, i));
            }}
            data-test={`sort-order-${i}`}
          >
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
  constructor(props) {
    super(props);
    this.state = { isWatching: false, intervalId: null };
    this.refreshData = this.refreshData.bind(this);
  }
  componentDidMount() {
    const dispatch = this.props.dispatch;
    dispatch(setDefaultQuery(this.props.curQuery));
  }

  componentWillUnmount() {
    clearInterval(this.state.intervalId);
  }

  watchChanges() {
    // set state on watch
    this.setState({ isWatching: !this.state.isWatching });
    if (this.state.isWatching) {
      clearInterval(this.state.intervalId);
    } else {
      const intervalId = setInterval(this.refreshData, 2000);
      this.setState({ intervalId: intervalId });
    }
  }

  refreshData() {
    this.props.dispatch(vMakeRequest());
  }

  render() {
    const { dispatch, whereAnd, triggerSchema, orderBy } = this.props;
    const styles = require('../../../Common/FilterQuery/FilterQuery.scss');
    return (
      <div>
        <form
          onSubmit={e => {
            e.preventDefault();
            dispatch(runQuery());
          }}
        >
          <div className="">
            <div
              className={`${styles.queryBox} col-xs-6 ${
                styles.padd_left_remove
              }`}
            >
              <span className={styles.subheading_text}>Filter</span>
              {renderWheres(whereAnd, triggerSchema, dispatch)}
            </div>
            <div
              className={`${styles.queryBox} col-xs-6 ${
                styles.padd_left_remove
              }`}
            >
              <b className={styles.subheading_text}>Sort</b>
              {renderSorts(orderBy, triggerSchema, dispatch)}
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
            <Button
              onClick={this.watchChanges.bind(this)}
              className={styles.add_mar_left}
              color="white"
              size="sm"
              data-test="run-query"
            >
              {this.state.isWatching ? (
                <span>
                  Watching <i className={'fa fa-spinner fa-spin'} />
                </span>
              ) : (
                'Watch'
              )}
            </Button>
          </div>
        </form>
      </div>
    );
  }
}

FilterQuery.propTypes = {
  curQuery: PropTypes.object.isRequired,
  triggerSchema: PropTypes.object.isRequired,
  whereAnd: PropTypes.array.isRequired,
  orderBy: PropTypes.array.isRequired,
  limit: PropTypes.number.isRequired,
  count: PropTypes.number,
  triggerName: PropTypes.string,
  offset: PropTypes.number.isRequired,
  dispatch: PropTypes.func.isRequired,
};

export default FilterQuery;
