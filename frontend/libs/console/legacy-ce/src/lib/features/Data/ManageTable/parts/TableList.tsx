import React, { useState } from 'react';
import { FaAngleLeft, FaAngleRight } from 'react-icons/fa';
import { Badge } from '../../../../new-components/Badge';
import { Button } from '../../../../new-components/Button';
import { CardedTable } from '../../../../new-components/CardedTable';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import {
  DEFAULT_PAGE_NUMBER,
  DEFAULT_PAGE_SIZE,
  DEFAULT_PAGE_SIZES,
} from '../../TrackResources/constants';
import { useCheckRows } from '../hooks/useCheckRows';
import { TrackableTable } from '../types';
import { paginate, search } from '../utils';
import { SearchBar } from './SearchBar';
import { TableRow } from './TableRow';
import { usePushRoute } from '../../../ConnectDBRedesign/hooks';
import { useTrackTables } from '../../hooks/useTrackTables';
import { hasuraToast } from '../../../../new-components/Toasts';
import { DisplayToastErrorMessage } from '../../components/DisplayErrorMessage';

interface TableListProps {
  dataSourceName: string;
  tables: TrackableTable[];
  mode: 'track' | 'untrack';
  onTrackedTable?: () => void;
}

export const TableList = (props: TableListProps) => {
  const { mode, dataSourceName, tables, onTrackedTable } = props;

  const [pageNumber, setPageNumber] = useState(DEFAULT_PAGE_NUMBER);
  const [pageSize, setPageSize] = useState(DEFAULT_PAGE_SIZE);
  const [searchText, setSearchText] = useState('');
  const filteredTables = search(tables, searchText);

  const checkboxRef = React.useRef<HTMLInputElement>(null);

  const { checkedIds, onCheck, allChecked, toggleAll, reset, inputStatus } =
    useCheckRows(filteredTables || []);

  React.useEffect(() => {
    if (!checkboxRef.current) return;
    checkboxRef.current.indeterminate = inputStatus === 'indeterminate';
  }, [inputStatus]);

  const { trackTables, isLoading, untrackTables } = useTrackTables({
    dataSourceName,
  });

  const onClick = async () => {
    const tables = filteredTables.filter(({ name }) =>
      checkedIds.includes(name)
    );

    if (mode === 'untrack') {
      trackTables({
        tablesToBeTracked: tables,
        onSuccess: () => {
          hasuraToast({
            type: 'success',
            title: 'Successfully tracked',
            message: `${tables.length} ${
              tables.length <= 1 ? 'table' : 'tables'
            } tracked!`,
          });
        },
        onError: err => {
          hasuraToast({
            type: 'error',
            title: err.name,
            children: <DisplayToastErrorMessage message={err.message} />,
          });
        },
      });
    } else {
      untrackTables({
        tablesToBeUntracked: tables,
        onSuccess: () => {
          hasuraToast({
            type: 'success',
            title: 'Successfully untracked',
            message: `${tables.length} ${
              tables.length <= 1 ? 'table' : 'tables'
            } untracked`,
          });
        },
        onError: err => {
          hasuraToast({
            type: 'error',
            title: err.name,
            children: <DisplayToastErrorMessage message={err.message} />,
          });
        },
      });
    }

    onTrackedTable?.();
    reset();
  };

  const pushRoute = usePushRoute();

  if (!tables.length) {
    return (
      <div className="space-y-4">
        <IndicatorCard>{`No ${
          mode === 'track' ? 'tracked' : 'untracked'
        } tables found`}</IndicatorCard>
      </div>
    );
  }
  return (
    <div className="space-y-4">
      <div className="flex justify-between space-x-4">
        <div className="flex gap-5">
          <Button
            mode="primary"
            disabled={!checkedIds.length}
            onClick={onClick}
            isLoading={isLoading}
            loadingText={'Please Wait'}
          >
            {`${mode === 'track' ? 'Untrack' : 'Track'} Selected (${
              checkedIds.length
            })`}
          </Button>
          <span className="border-r border-slate-300"></span>
          <div className="flex gap-2">
            <SearchBar onSearch={data => setSearchText(data)} />
            {searchText.length ? (
              <Badge>{filteredTables.length} results found</Badge>
            ) : null}
          </div>
        </div>

        <div className="flex gap-1">
          <Button
            icon={<FaAngleLeft />}
            onClick={() => setPageNumber(pageNumber - 1)}
            disabled={pageNumber === 1}
          />
          <select
            value={pageSize}
            onChange={e => {
              setPageSize(Number(e.target.value));
            }}
            className="block w-full max-w-xl h-8 min-h-full shadow-sm rounded pl-3 pr-6 py-0.5 border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400"
          >
            {DEFAULT_PAGE_SIZES.map(_pageSize => (
              <option key={_pageSize} value={_pageSize}>
                Show {_pageSize} tables
              </option>
            ))}
          </select>
          <Button
            icon={<FaAngleRight />}
            onClick={() => setPageNumber(pageNumber + 1)}
            disabled={pageNumber >= filteredTables.length / pageSize}
          />
        </div>
      </div>
      <CardedTable.Table>
        <CardedTable.TableHead>
          <CardedTable.TableHeadRow>
            <th className="w-0 bg-gray-50 px-sm text-sm font-semibold text-muted uppercase tracking-wider border-r">
              <input
                ref={checkboxRef}
                type="checkbox"
                className="cursor-pointer
                rounded border shadow-sm border-gray-400 hover:border-gray-500 focus:ring-yellow-400"
                checked={allChecked}
                onChange={toggleAll}
              />
            </th>
            <CardedTable.TableHeadCell>Object</CardedTable.TableHeadCell>
            <CardedTable.TableHeadCell>Type</CardedTable.TableHeadCell>
            <CardedTable.TableHeadCell>Actions</CardedTable.TableHeadCell>
          </CardedTable.TableHeadRow>
        </CardedTable.TableHead>

        <CardedTable.TableBody>
          {paginate(filteredTables, pageSize, pageNumber).map(table => (
            <TableRow
              key={table.id}
              table={table}
              dataSourceName={dataSourceName}
              checked={checkedIds.includes(table.id)}
              reset={reset}
              onChange={() => onCheck(table.id)}
              onTableNameClick={
                mode === 'track'
                  ? () => {
                      pushRoute(
                        `data/v2/manage/table/browse?database=${dataSourceName}&table=${encodeURIComponent(
                          JSON.stringify(table.table)
                        )}`
                      );
                    }
                  : undefined
              }
            />
          ))}
        </CardedTable.TableBody>
      </CardedTable.Table>
    </div>
  );
};
