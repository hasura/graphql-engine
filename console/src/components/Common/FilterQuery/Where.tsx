import React from 'react';
import { ValueFilter, Operator } from './Types';
import { allOperators } from './utils';

import { BaseTable } from '../utils/pgUtils';
import { isNull } from '../utils/jsUtils';
import styles from './FilterQuery.scss';

type WhereProps = {
  filters: ValueFilter[];
  setFilters: (f: ValueFilter[]) => void;
  table: BaseTable;
};

const Where = (props: WhereProps) => {
  const { filters, setFilters, table } = props;

  return (
    <React.Fragment>
      {filters.map((filter, i) => {
        const removeFilter = () => {
          setFilters([...filters.slice(0, i), ...filters.slice(i + 1)]);
        };

        const setKey = (e: React.BaseSyntheticEvent) => {
          const col = e.target.value;
          setFilters([
            ...filters.slice(0, i),
            { ...filters[i], key: col },
            ...filters.slice(i + 1),
          ]);
        };

        const setOperator = (e: React.BaseSyntheticEvent) => {
          const op: Operator = e.target.value;
          setFilters([
            ...filters.slice(0, i),
            { ...filters[i], operator: op, value: '' },
            ...filters.slice(i + 1),
          ]);
        };

        const setValue = (e: React.BaseSyntheticEvent) => {
          const value = e.target.value;
          setFilters([
            ...filters.slice(0, i),
            { ...filters[i], value },
            ...filters.slice(i + 1),
          ]);
        };

        const removeIcon =
          filters.length === i + 1 ? null : (
            <i
              className="fa fa-times"
              onClick={removeFilter}
              data-test={`clear-filter-${i}`}
            />
          );

        return (
          <div
            key={i} // eslint-disable-line react/no-array-index-key
            className={`${styles.inputRow} row`}
          >
            <div className="col-xs-4">
              <select
                className="form-control"
                onChange={setKey}
                value={filter.key}
                data-test={`filter-column-${i}`}
              >
                {filter.key === '' ? (
                  <option disabled value="">
                    -- column --
                  </option>
                ) : null}
                {table.columns.map(c => (
                  <option key={c.column_name} value={c.column_name}>
                    {c.column_name}
                  </option>
                ))}
              </select>
            </div>
            <div className="col-xs-3">
              <select
                className="form-control"
                onChange={setOperator}
                value={filter.operator || ''}
                data-test={`filter-op-${i}`}
              >
                {isNull(filter.operator) ? (
                  <option disabled value="">
                    -- op --
                  </option>
                ) : null}
                {allOperators.map(o => (
                  <option key={o.operator} value={o.operator}>
                    {`[${o.alias}] ${o.name}`}
                  </option>
                ))}
              </select>
            </div>
            <div className="col-xs-4">
              <input
                className="form-control"
                placeholder="-- value --"
                value={filter.value}
                onChange={setValue}
                data-test={`filter-value-${i}`}
              />
            </div>
            <div className="text-center col-xs-1">{removeIcon}</div>
          </div>
        );
      })}
    </React.Fragment>
  );
};

export default Where;
