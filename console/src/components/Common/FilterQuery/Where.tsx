import React from 'react';
import { ValueFilter, Operator } from './types';
import { allOperators } from './utils';

import { isNotDefined } from '../utils/jsUtils';
import styles from './FilterQuery.scss';
import { BaseTable } from '../../../dataSources/types';

type Props = {
  filters: ValueFilter[];
  setFilters: (f: ValueFilter[]) => void;
  table: BaseTable;
};

const Where: React.FC<Props> = props => {
  const { filters, setFilters, table } = props;

  return (
    <React.Fragment>
      {filters.map((filter, i) => {
        const removeFilter = () => {
          setFilters([...filters.slice(0, i), ...filters.slice(i + 1)]);
        };

        const setKey = (e: React.ChangeEvent<HTMLSelectElement>) => {
          const col = e.target.value;
          setFilters([
            ...filters.slice(0, i),
            { ...filters[i], key: col },
            ...filters.slice(i + 1),
          ]);
        };

        const setOperator = (e: React.BaseSyntheticEvent) => {
          // TODO synthetic event with enums
          const op: Operator = e.target.value;
          setFilters([
            ...filters.slice(0, i),
            { ...filters[i], operator: op, value: '' },
            ...filters.slice(i + 1),
          ]);
        };

        const setValue = (e: React.ChangeEvent<HTMLInputElement>) => {
          const value = e.target.value;
          setFilters([
            ...filters.slice(0, i),
            { ...filters[i], value },
            ...filters.slice(i + 1),
          ]);
        };

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
                {isNotDefined(filter.operator) ? (
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
            <div className="text-center col-xs-1">
              {filters.length === i + 1 ? null : (
                <i
                  className="fa fa-times"
                  onClick={removeFilter}
                  data-test={`clear-filter-${i}`}
                />
              )}
            </div>
          </div>
        );
      })}
    </React.Fragment>
  );
};

export default Where;
