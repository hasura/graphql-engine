import { renderHook, act } from '@testing-library/react-hooks';
import { usePaginatedSearchableList } from './usePaginatedSearchableList';

const mockData = [
  { id: '1', name: 'John' },
  { id: '2', name: 'Jane' },
  { id: '3', name: 'Bob' },
  { id: '4', name: 'Alice' },
  { id: '5', name: 'Eve' },
  { id: '6', name: 'Carol' },
  { id: '7', name: 'Dave' },
  { id: '8', name: 'Frank' },
  { id: '9', name: 'Grace' },
  { id: '10', name: 'Heidi' },
  { id: '11', name: 'Ivan' },
  { id: '12', name: 'Mallory' },
  { id: '13', name: 'Oscar' },
  { id: '14', name: 'Peggy' },
  { id: '15', name: 'Trent' },
];

describe('usePaginatedSearchableList', () => {
  it('should return the correct initial state', () => {
    const { result } = renderHook(() =>
      usePaginatedSearchableList({
        data: mockData,
        filterFn: (searchText, item) => item.name.includes(searchText),
      })
    );

    expect(result.current.pageNumber).toBe(1);
    expect(result.current.pageSize).toBe(10);
    expect(result.current.searchIsActive).toBe(false);
    expect(result.current.filteredData).toEqual(mockData);
    expect(result.current.paginatedData).toEqual(mockData.slice(0, 10));
    expect(result.current.totalPages).toBe(2);
    expect(result.current.checkData.checkedIds).toEqual([]);
    expect(result.current.getCheckedItems()).toEqual([]);
    expect(result.current.dataSize).toBe(mockData.length);
  });

  it('should go to page 2 and display page 2 data when navigating page', () => {
    const { result } = renderHook(() =>
      usePaginatedSearchableList({
        data: mockData,
        filterFn: (searchText, item) => item.name.includes(searchText),
      })
    );

    act(() => {
      result.current.incrementPage();
    });

    expect(result.current.pageNumber).toBe(2);
    expect(result.current.filteredData).toEqual(mockData);
    expect(result.current.paginatedData).toEqual(mockData.slice(10, 15));
    expect(result.current.totalPages).toBe(2);
    expect(result.current.dataSize).toBe(mockData.length);
  });

  it('should update the filtered data when search text changes', () => {
    const { result } = renderHook(() =>
      usePaginatedSearchableList({
        data: mockData,
        filterFn: (searchText, item) => item.name.includes(searchText),
      })
    );

    act(() => {
      result.current.incrementPage();
      result.current.handleSearch('o');
    });

    expect(result.current.searchIsActive).toBe(true);
    expect(result.current.pageNumber).toBe(1);
    expect(result.current.totalPages).toBe(1);
    expect(result.current.dataSize).toBe(4);
  });

  it('should go to page 1 when data are filtered on page 2 and filtered data length < page size', () => {
    const { result } = renderHook(() =>
      usePaginatedSearchableList({
        data: mockData,
        filterFn: (searchText, item) => item.name.includes(searchText),
      })
    );

    act(() => {
      result.current.handleSearch('o');
    });

    expect(result.current.searchIsActive).toBe(true);
    expect(result.current.filteredData).toEqual([
      { id: '1', name: 'John' },
      { id: '3', name: 'Bob' },
      { id: '6', name: 'Carol' },
      { id: '12', name: 'Mallory' },
    ]);
    expect(result.current.paginatedData).toEqual([
      { id: '1', name: 'John' },
      { id: '3', name: 'Bob' },
      { id: '6', name: 'Carol' },
      { id: '12', name: 'Mallory' },
    ]);
    expect(result.current.pageNumber).toBe(1);
    expect(result.current.totalPages).toBe(1);
    expect(result.current.dataSize).toBe(4);
  });

  it('should update the paginated data when page number changes', () => {
    const { result } = renderHook(() =>
      usePaginatedSearchableList({
        data: mockData,
        filterFn: (searchText, item) => item.name.includes(searchText),
      })
    );

    act(() => {
      result.current.setPageNumber(2);
    });

    expect(result.current.pageNumber).toBe(2);
    expect(result.current.paginatedData).toEqual([
      { id: '11', name: 'Ivan' },
      { id: '12', name: 'Mallory' },
      { id: '13', name: 'Oscar' },
      { id: '14', name: 'Peggy' },
      { id: '15', name: 'Trent' },
    ]);
    expect(result.current.totalPages).toBe(2);
    expect(result.current.dataSize).toBe(mockData.length);
  });

  it('should update the checked items when rows are checked', () => {
    const { result } = renderHook(() =>
      usePaginatedSearchableList({
        data: mockData,
        filterFn: (searchText, item) => item.name.includes(searchText),
      })
    );

    act(() => {
      result.current.checkData.onCheck('1');
    });

    expect(result.current.checkData.checkedIds).toEqual(['1']);
    expect(result.current.getCheckedItems()).toEqual([
      { id: '1', name: 'John' },
    ]);
    expect(result.current.dataSize).toBe(mockData.length);
  });
});
