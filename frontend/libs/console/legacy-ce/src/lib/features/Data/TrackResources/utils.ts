type PaginateProps<TData> = {
  data: TData[];
  pageNumber: number;
  pageSize: number;
};

type PaginationResult<T> = {
  data: T[];
  totalPages: number;
};

export const paginate = <T>({
  pageNumber,
  pageSize,
  data,
}: PaginateProps<T>): PaginationResult<T> => {
  const startIndex = (pageNumber - 1) * pageSize;
  const endIndex = startIndex + pageSize;
  const paginatedData = data.slice(startIndex, endIndex);
  const totalPages = Math.ceil(data.length / pageSize);
  return { data: paginatedData, totalPages };
};

export function search<T>({
  data,
  searchText,
  filterFn,
}: {
  data: T[];
  searchText: string;
  filterFn: (searchText: string, item: T) => boolean;
}) {
  return data.filter(item => filterFn(searchText, item));
}

export const filterByText = (parentText: string, searchText: string) => {
  if (!searchText.length) return true;

  return parentText.includes(searchText.toLowerCase());
};

export const filterByTableType = (type: string, selectedTypes?: string[]) => {
  if (!selectedTypes?.length) return true;

  return selectedTypes.includes(type);
};
