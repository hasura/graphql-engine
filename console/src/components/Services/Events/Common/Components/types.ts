import {
  FilterState,
  SetFilterState,
  RunQuery,
} from '../../../../Common/FilterQuery/types';

export type FilterTableProps = {
  rows: any[];
  count?: number;
  filterState: FilterState;
  setFilterState: SetFilterState;
  runQuery: RunQuery;
  columns: string[];
  identifier: string;
};
