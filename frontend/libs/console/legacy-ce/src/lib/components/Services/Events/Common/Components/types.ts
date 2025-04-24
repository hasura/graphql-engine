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

export type GridHeadingProps = {
  Header: string | React.ReactNode;
  accessor: string;
  id?: string;
  width?: number;
  expander?: boolean;
  Expander?: React.FC<{
    isExpanded: boolean;
    viewIndex: number;
  }>;
};
