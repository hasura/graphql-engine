import React, { useEffect, useState, useMemo } from 'react';
import { ThunkDispatch } from 'redux-thunk';
import { AnyAction } from 'redux';
import { createHistory } from 'history';

import { Operators } from '../constants';
import {
  setFilterCol,
  setFilterOp,
  setFilterVal,
  addFilter,
  removeFilter,
  setOrderCol,
  setOrderType,
  addOrder,
  removeOrder,
  setDefaultQuery,
  runQuery,
  setOffset,
  setFilterColumns,
} from './FilterActions.js';
import Button from '../../../Common/Button/Button';
import ReloadEnumValuesButton from '../Common/Components/ReloadEnumValuesButton';
import styles from '../../../Common/FilterQuery/FilterQuery.scss';
import {
  getPersistedPageSize,
  getPersistedSelectedColumns,
  persistSelectedColumns,
} from './localStorageUtils';
import { isEmpty } from '../../../Common/utils/jsUtils';
import { getDefaultValue, getQueryFromUrl } from './utils';
import { ColumnsSelector } from './ColumnsSelector';
import CollapsibleToggle from '../../../Common/CollapsibleToggle/CollapsibleToggle';
import { UNSAFE_keys } from '../../../Common/utils/tsUtils';

const history = createHistory();

type OrderBy = {
  column: string;
  type: string;
  nulls: string;
}[];
type Where = {
  $and: Record<string, unknown>[];
}[];

type ColumnsProps = {
  colName: string;
  tableSchema: { columns: { column_name: string }[] };
  onChange: (e: React.ChangeEvent<HTMLSelectElement>) => void;
  usage: 'filter' | 'sort';
  index: number;
  skipColumns: string[];
};
const Columns: React.FC<ColumnsProps> = ({
  colName,
  tableSchema,
  onChange,
  usage,
  index,
  skipColumns,
}) => {
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
        usage === 'sort' ? `sort-column-${index}` : `filter-column-${index}`
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

type OptionsProps = {
  opName: string;
  onChange: (e: React.ChangeEvent<HTMLSelectElement>) => void;
  index: number;
};
const Options: React.FC<OptionsProps> = ({ opName, onChange, index }) => (
  <select
    className="form-control"
    onChange={onChange}
    value={opName.trim()}
    data-test={`filter-op-${index}`}
  >
    {opName.trim() === '' ? (
      <option disabled value="">
        -- op --
      </option>
    ) : null}
    {Operators.map((o, i) => (
      <option key={i} value={o.value}>
        {`[${o.graphqlOp}] ${o.name}`}
      </option>
    ))}
  </select>
);

type WhereSectionProps = {
  whereAnd: Where;
  tableSchema: any;
  dispatch: ThunkDispatch<{}, {}, AnyAction>;
};
const WhereSection = ({
  whereAnd,
  tableSchema,
  dispatch,
}: WhereSectionProps) => {
  return (
    <>
      {whereAnd.map((clause, i) => {
        const colName = UNSAFE_keys(clause)[0];
        const opName = Object.keys(clause[colName])[0];

        const dSetFilterCol = (e: React.ChangeEvent<HTMLSelectElement>) => {
          dispatch(setFilterCol(e.target.value, i));
        };
        const dSetFilterOp = (e: React.ChangeEvent<HTMLSelectElement>) => {
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
          <div
            key={i}
            className={`${styles.inputRow} ${styles.remove_margin_top} row`}
          >
            <div className="col-xs-4">
              <Columns
                colName={colName}
                tableSchema={tableSchema}
                onChange={dSetFilterCol}
                usage="filter"
                index={i}
                skipColumns={[]}
              />
            </div>
            <div className="col-xs-3">
              <Options opName={opName} onChange={dSetFilterOp} index={i} />
            </div>
            <div className="col-xs-4">
              <input
                className="form-control"
                placeholder="-- value --"
                value={
                  getDefaultValue(
                    (clause[colName] as Record<string, any>)[opName],
                    opName
                  ) as string[]
                }
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
      })}
    </>
  );
};

type SortSectionProps = {
  orderBy: OrderBy;
  tableSchema: any;
  dispatch: ThunkDispatch<{}, {}, AnyAction>;
};
const SortSection = ({ orderBy, tableSchema, dispatch }: SortSectionProps) => {
  const currentOrderBy = orderBy.map(o => o.column);
  return (
    <>
      {orderBy.map((c, i) => {
        const dSetOrderCol = (e: React.ChangeEvent<HTMLSelectElement>) => {
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
          <div
            key={i}
            className={`${styles.inputRow} ${styles.remove_margin_top} row`}
          >
            <div className="col-xs-6">
              <Columns
                colName={c.column}
                tableSchema={tableSchema}
                onChange={dSetOrderCol}
                usage="sort"
                index={i}
                skipColumns={currentOrderBy}
              />
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
      })}
    </>
  );
};

const setParams = (
  query: { filters: string[]; sorts: string[] } = { filters: [], sorts: [] }
) => {
  const searchParams = new URLSearchParams();
  query.filters.forEach(filter => searchParams.append('filter', filter));
  query.sorts.forEach(sort => searchParams.append('sort', sort));
  return searchParams.toString();
};

const setUrlParams = (whereQuery: Where, orderByQuery: OrderBy) => {
  const sorts = orderByQuery
    .filter(order => order.column)
    .map(order => `${order.column};${order.type}`);
  const filters = whereQuery
    .filter(
      where => Object.keys(where).length === 1 && Object.keys(where)[0] !== ''
    )
    .map(where => {
      const col = UNSAFE_keys(where)[0];
      const op = Object.keys(where[col])[0];
      const value = (where[col] as Record<string, any>)[op];
      return `${col};${op};${value}`;
    });
  const url = setParams({ filters, sorts });
  history.push({
    pathname: history.getCurrentLocation().pathname,
    search: `?${url}`,
  });
};

const Title = ({ title, summary }: { title: string; summary: string }) => (
  <div className={styles.display_flex}>
    <h2 className={`${styles.subheading_text} ${styles.padd_bottom}`}>
      {title}
    </h2>
    <i className={styles.summaryLabel}>{summary}</i>
  </div>
);

export interface FilterQueryProps {
  tableSchema: {
    table_name: string;
    table_schema: string;
    is_enum: boolean;
  };
  dispatch: ThunkDispatch<{}, {}, AnyAction>;
  curQuery: {
    limit?: number;
    offset?: number;
    order_by?: OrderBy;
    where?: Where;
  };
  urlQuery?: {
    filter: string;
    sort: string;
  };
  whereAnd: Where;
  orderBy: OrderBy;
  allColumns: string[];
  columns?: string[];
}
const FilterQuery: React.FC<FilterQueryProps> = ({
  curQuery,
  urlQuery,
  dispatch,
  whereAnd,
  tableSchema,
  orderBy,
  allColumns,
  columns,
}) => {
  const [selectedColumns, setSelectedColumns] = useState<string[]>([]);
  const [isFiltersPanelOpen, setFiltersPanelOpen] = useState(false);
  const [isSortPanelOpen, setSortPanelOpen] = useState(false);
  const [isColumnsPanelOpen, setColumnsPanelOpen] = useState(false);

  useEffect(() => {
    setSelectedColumns([]);
    setFiltersPanelOpen(false);
    setSortPanelOpen(false);
    setColumnsPanelOpen(false);
  }, [tableSchema.table_name]);

  useEffect(() => {
    if (columns && columns.length) {
      setSelectedColumns(columns);
    } else {
      setSelectedColumns(allColumns);
    }
  }, [allColumns, columns]);

  useEffect(() => {
    const limit = getPersistedPageSize();
    const persistedColumns = getPersistedSelectedColumns(
      tableSchema.table_name,
      tableSchema.table_schema
    );
    if (isEmpty(urlQuery)) {
      dispatch(
        setDefaultQuery({ ...curQuery, limit, columns: persistedColumns })
      );
      return;
    }

    const query = getQueryFromUrl(urlQuery!);

    dispatch(setDefaultQuery({ ...query, limit, columns: persistedColumns }));
    dispatch(runQuery(tableSchema));
  }, []);

  const filtersSummary = useMemo(() => {
    if (!whereAnd || whereAnd.length < 2) return 'no filters applied';
    const summary = whereAnd.reduce((acc, where) => {
      const column = Object.keys(where || {})[0];
      if (!column) return acc;
      return `${acc ? `${acc},` : acc} ${column}`;
    }, '');
    console.log({ whereAnd });
    return summary;
  }, [whereAnd]);

  const sortsSummary = useMemo(() => {
    if (!orderBy || orderBy.length < 2) return 'no idea for placeholder';
    const summary = orderBy.reduce((acc, { column, type }) => {
      if (!column) return acc;
      return `${acc ? `${acc},` : acc} ${column}: ${type}`;
    }, '');
    return summary;
  }, [orderBy]);

  const onSubmit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    dispatch(setOffset(0));
    setUrlParams(whereAnd, orderBy);
    dispatch(setFilterColumns(selectedColumns));
    persistSelectedColumns(
      tableSchema.table_name,
      tableSchema.table_schema,
      selectedColumns
    );
    dispatch(runQuery(tableSchema));
  };

  return (
    <form className={styles.add_mar_top} onSubmit={onSubmit}>
      <div
        className={styles.displayFlexContainer}
        style={{ flexDirection: 'column' }}
      >
        <div
          className={`${styles.queryBox} col-xs-6 ${styles.padd_left_remove}`}
        >
          <CollapsibleToggle
            title={<Title title="Filters" summary={filtersSummary} />}
            isOpen={isFiltersPanelOpen}
            toggleHandler={() => setFiltersPanelOpen(prev => !prev)}
          >
            <WhereSection
              whereAnd={whereAnd}
              tableSchema={tableSchema}
              dispatch={dispatch}
            />
          </CollapsibleToggle>
        </div>
        <div
          className={`${styles.queryBox} col-xs-6 ${styles.padd_left_remove}`}
        >
          <CollapsibleToggle
            title={<Title title="Sort" summary={sortsSummary} />}
            isOpen={isSortPanelOpen}
            toggleHandler={() => setSortPanelOpen(prev => !prev)}
          >
            <SortSection
              orderBy={orderBy}
              tableSchema={tableSchema}
              dispatch={dispatch}
            />
          </CollapsibleToggle>
        </div>
      </div>
      <div className={`${styles.queryBox} col-xs-6 ${styles.padd_left_remove}`}>
        <ColumnsSelector
          allColumns={allColumns}
          selectedColumns={selectedColumns}
          setSelected={setSelectedColumns}
          isOpen={isColumnsPanelOpen}
          setIsOpen={setColumnsPanelOpen}
        />
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
        <ReloadEnumValuesButton
          dispatch={dispatch}
          isEnum={tableSchema.is_enum}
          tooltipStyle={styles.add_mar_left_mid}
        />
      </div>
    </form>
  );
};

export default FilterQuery;
