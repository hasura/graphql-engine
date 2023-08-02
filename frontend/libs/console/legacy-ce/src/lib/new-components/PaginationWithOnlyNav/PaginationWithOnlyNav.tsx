import { Button } from '../Button';
import { inputStyles } from '../../components/Services/Events/constants';

export type PaginationWithOnlyNavProps = {
  offset: number;
  limit: number;
  changePage: (value: number) => void;
  changePageSize: (value: number) => void;
  rows: any[];
};

export const PaginationWithOnlyNav = (props: PaginationWithOnlyNavProps) => {
  const { offset, limit, changePage, changePageSize, rows } = props;
  const newPage = offset / limit;
  const isNextEnabled = rows.length === limit;
  return (
    <div className="flex ml-sm mr-sm mb-sm mt-sm justify-around max-w-5xl">
      <div>
        <Button
          onClick={() => changePage(newPage - 1)}
          disabled={offset === 0}
          data-test="custom-pagination-prev"
        >
          Prev
        </Button>
      </div>
      <div className="w-1/3">
        <select
          className={inputStyles}
          value={limit}
          onChange={e => {
            e.persist();
            changePageSize(parseInt(e.target.value, 10) || 10);
          }}
          data-test="pagination-select"
        >
          <option disabled value="">
            --
          </option>
          <option value={5}>5 rows</option>
          <option value={10}>10 rows</option>
          <option value={20}>20 rows</option>
          <option value={25}>25 rows</option>
          <option value={50}>50 rows</option>
          <option value={100}>100 rows</option>
        </select>
      </div>
      <div>
        <Button
          onClick={() => changePage(newPage + 1)}
          disabled={rows.length === 0 || !isNextEnabled}
          data-test="custom-pagination-next"
        >
          Next
        </Button>
      </div>
    </div>
  );
};
