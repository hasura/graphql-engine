import { FaChevronLeft, FaChevronRight } from 'react-icons/fa6';
import { IconButton } from '../../Button';
import { Flex, FlexProps } from '@radix-ui/themes';
import { Select, SelectItemProps } from '../../Form';
import { safeParseInt } from '@hasura/shared/utils';
import { Text } from '../../typography';

export type PaginationOffsetProps = FlexProps & {
  offset: number;
  limit: number;
  changePage: (value: number) => void;
  changePageSize: (value: number) => void;
  rows: any[];
};

const defaultOptions: SelectItemProps[] = [5, 10, 20, 25, 50, 100].map(
  (value) => ({
    label: `${value} rows`,
    value: value.toString(),
  }),
);

export const PaginationOffset = ({
  offset,
  limit,
  changePage,
  changePageSize,
  rows,
  ...rest
}: PaginationOffsetProps) => {
  const pageIndex = offset / limit;
  const isNextEnabled = rows.length === limit;

  return (
    <Flex align="center" className="p-2" gap="2" {...rest}>
      <IconButton
        variant="ghost"
        onClick={() => changePage(pageIndex - 1)}
        disabled={offset === 0}
        data-test="custom-pagination-prev"
      >
        <FaChevronLeft />
      </IconButton>
      <Select
        placeholder="-- Size --"
        value={limit ? limit.toString() : undefined}
        options={defaultOptions}
        onChange={(value) => {
          changePageSize(safeParseInt(value, 20));
        }}
      />
      <Text>Page {pageIndex + 1}</Text>
      <IconButton
        variant="ghost"
        onClick={() => changePage(pageIndex + 1)}
        disabled={rows.length === 0 || !isNextEnabled}
        data-test="custom-pagination-next"
      >
        <FaChevronRight />
      </IconButton>
    </Flex>
  );
};
