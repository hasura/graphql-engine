import React from 'react';
import { OrderBy } from '../utils/v1QueryUtils';

import styles from './FilterQuery.scss';
import { BaseTable } from '../../../dataSources/types';

type Props = {
  sorts: OrderBy[];
  setSorts: (o: OrderBy[]) => void;
  table: BaseTable;
};

const Sorts: React.FC<Props> = props => {
  const { sorts, setSorts, table } = props;

  return (
    <React.Fragment>
      {sorts.map((sort, i) => {
        const removeSort = () => {
          setSorts([...sorts.slice(0, i), ...sorts.slice(i + 1)]);
        };

        const setColumn = (e: React.ChangeEvent<HTMLSelectElement>) => {
          const col = e.target.value;
          setSorts([
            ...sorts.slice(0, i),
            { ...sorts[i], column: col },
            ...sorts.slice(i + 1),
          ]);
        };

        const setType = (e: React.BaseSyntheticEvent) => {
          const type = e.target.value;
          setSorts([
            ...sorts.slice(0, i),
            { ...sorts[i], type },
            ...sorts.slice(i + 1),
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
                onChange={setColumn}
                value={sort.column}
                data-test={`filter-column-${i}`}
              >
                {sort.column === '' ? (
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
                onChange={setType}
                value={sort.type}
                data-test={`filter-op-${i}`}
              >
                <option key="asc" value="asc">
                  asc
                </option>
                <option key="desc" value="desc">
                  desc
                </option>
              </select>
            </div>
            <div className="text-center col-xs-1">
              {sorts.length === i + 1 ? null : (
                <i
                  className="fa fa-times"
                  onClick={removeSort}
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

export default Sorts;
