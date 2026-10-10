import { IconButton, Select, Text } from '@hasura/shared/ui';
import { DEFAULT_PAGE_SIZES } from '../constants';
import {
  FaAngleDoubleLeft,
  FaAngleDoubleRight,
  FaAngleLeft,
  FaAngleRight,
} from 'react-icons/fa';
import { PaginatedSearchableListProps } from '../hooks/usePaginatedSearchableList';
import { Flex } from '@radix-ui/themes';

const selectOptions = DEFAULT_PAGE_SIZES.map((pageSize) => ({
  label: `Show ${pageSize} items`,
  value: String(pageSize),
}));

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
  <Flex gap="1" align="center">
    <Text className="whitespace-nowrap mr-2">
      Page {pageNumber} of {totalPages}
    </Text>
    <IconButton
      mode="default"
      onClick={goToFirstPage}
      disabled={pageNumber === 1}
    >
      <FaAngleDoubleLeft />
    </IconButton>
    <IconButton
      mode="default"
      onClick={decrementPage}
      disabled={pageNumber === 1}
    >
      <FaAngleLeft />
    </IconButton>
    <Select
      value={pageSize.toString()}
      onChange={(value) => {
        setPageSize(Number(value));
      }}
      options={selectOptions}
    />
    <IconButton
      mode="default"
      onClick={incrementPage}
      disabled={pageNumber >= dataSize / pageSize}
    >
      <FaAngleRight />
    </IconButton>
    <IconButton
      mode="default"
      onClick={goToLastPage}
      disabled={pageNumber >= dataSize / pageSize}
    >
      <FaAngleDoubleRight />
    </IconButton>
  </Flex>
);
