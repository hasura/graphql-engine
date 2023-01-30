import { convertRowsToCSV, convertRowsToJSON } from './export.utils';

describe('convertRowsToCSV', () => {
  describe('when data is provided', () => {
    it('returns a CSV', () => {
      const rows: Record<string, unknown>[] = [
        { id: 1, name: 'john' },
        { id: 2, name: 'jane' },
      ];
      expect(convertRowsToCSV(rows)).toBe(
        `data:text/csv;charset=utf-8,id%2Cname%0A%221%22%2C%22'john'%22%0A%222%22%2C%22'jane'%22`
      );
    });
  });

  describe('when data is not provided', () => {
    it('returns an empty CSV', () => {
      expect(convertRowsToCSV(undefined)).toBe(
        `data:text/csv;charset=utf-8,%0A`
      );
    });
  });
});

describe('convertRowsToJSON', () => {
  describe('when data is provided', () => {
    it('returns a JSON', () => {
      const rows = [
        { id: 1, name: 'john' },
        { id: 2, name: 'jane' },
      ];
      expect(convertRowsToJSON(rows)).toBe(
        `data:application/json;charset=utf-8;,%5B%0A%20%20%7B%0A%20%20%20%20%22id%22%3A%201%2C%0A%20%20%20%20%22name%22%3A%20%22john%22%0A%20%20%7D%2C%0A%20%20%7B%0A%20%20%20%20%22id%22%3A%202%2C%0A%20%20%20%20%22name%22%3A%20%22jane%22%0A%20%20%7D%0A%5D`
      );
    });
  });
});
