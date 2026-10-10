import { IconButton } from '../../Button';
import { Flex, FlexProps } from '@radix-ui/themes';
import { Select, SelectItemProps } from '../../Form';
import { safeParseInt } from '@hasura/shared/utils';
import { Text } from '../../typography';
import {
  ReactTable,
  TableFeature,
  TableState_RowPagination,
} from '@tanstack/react-table';
import { FaChevronLeft, FaChevronRight } from 'react-icons/fa6';

export type PaginationProps = {
  pageSize: number;
  pageIndex: number;
  pageCount?: number;
  justify?: FlexProps['justify'];
  setPageSize: (value: number) => void;
} & (
  | {
      setPageIndex: (value: number) => void;
    }
  | {
      goToPreviousPage: () => void;
      goToNextPage: () => void;
      canNext: boolean;
    }
);

export function getReactTablePaginationProps(
  table: Pick<
    ReactTable<
      {
        rowPaginationFeature: TableFeature;
      },
      Record<string, any>,
      TableState_RowPagination
    >,
    'state' | 'getPageCount' | 'setPageIndex' | 'setPageSize'
  >,
): PaginationProps {
  return {
    pageSize: table.state.pagination.pageSize,
    pageIndex: table.state.pagination.pageIndex,
    pageCount: table.getPageCount(),
    setPageIndex: table.setPageIndex,
    setPageSize: table.setPageSize,
  };
}

const defaultOptions: SelectItemProps[] = [5, 10, 20, 25, 50, 100].map(
  (value) => ({
    label: `Show ${value} rows`,
    value: value.toString(),
  }),
);

export const Pagination = ({
  pageIndex,
  pageSize,
  setPageSize,
  pageCount,
  justify,
  ...rest
}: PaginationProps) => {
  const isNextEnabled =
    'canNext' in rest ? rest.canNext : pageCount && pageIndex < pageCount - 1;
  const goToPreviousPage =
    'setPageIndex' in rest
      ? () => rest.setPageIndex(pageIndex - 1)
      : rest.goToPreviousPage;

  const goToNextPage =
    'setPageIndex' in rest
      ? () => rest.setPageIndex(pageIndex + 1)
      : rest.goToNextPage;
  return (
    <Flex align="center" className="p-2" gap="2" justify={justify}>
      <Text>
        Page {pageIndex + 1}
        {pageCount ? ` of ${pageCount}` : ''}
      </Text>
      <IconButton
        mode="default"
        onClick={goToPreviousPage}
        disabled={pageSize === 0 || !pageIndex}
        data-test="custom-pagination-prev"
      >
        <FaChevronLeft />
      </IconButton>
      <Select
        placeholder="-- Size --"
        value={pageSize ? pageSize.toString() : undefined}
        options={defaultOptions}
        onChange={(value) => {
          setPageSize(safeParseInt(value, 20));
        }}
      />
      <IconButton
        mode="default"
        onClick={goToNextPage}
        disabled={pageSize === 0 || !isNextEnabled}
        data-test="custom-pagination-next"
      >
        <FaChevronRight />
      </IconButton>
    </Flex>
  );
};
