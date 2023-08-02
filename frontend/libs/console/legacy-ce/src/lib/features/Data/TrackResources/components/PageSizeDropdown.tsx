import React from 'react';
import { Button } from '../../../../new-components/Button';
import { DEFAULT_PAGE_SIZES } from '../constants';
import {
  FaAngleDoubleLeft,
  FaAngleDoubleRight,
  FaAngleLeft,
  FaAngleRight,
} from 'react-icons/fa';
import { PaginatedSearchableListProps } from '../hooks/usePaginatedSearchableList';

export const PageSizeDropdown = ({
  pageNumber,
  pageSize,
  decrementPage,
  incrementPage,
  goToFirstPage,
  goToLastPage,
  setPageSize,
  dataSize,
  totalPages,
}: PaginatedSearchableListProps) => (
  <div className="flex gap-1 items-center">
    <span className="whitespace-nowrap mr-2">
      Page {pageNumber} of {totalPages}
    </span>
    <Button
      icon={<FaAngleDoubleLeft />}
      onClick={goToFirstPage}
      disabled={pageNumber === 1}
    />
    <Button
      icon={<FaAngleLeft />}
      onClick={decrementPage}
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
          Show {_pageSize} items
        </option>
      ))}
    </select>
    <Button
      icon={<FaAngleRight />}
      onClick={incrementPage}
      disabled={pageNumber >= dataSize / pageSize}
    />
    <Button
      icon={<FaAngleDoubleRight />}
      onClick={goToLastPage}
      disabled={pageNumber >= dataSize / pageSize}
    />
  </div>
);
