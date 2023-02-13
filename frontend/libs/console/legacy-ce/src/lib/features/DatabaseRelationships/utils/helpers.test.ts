import { getTableDisplayName } from './helpers';

describe('getTableDisplayName', () => {
  describe('when table is array', () => {
    it('returns the table name', () => {
      expect(getTableDisplayName(['public', 'name'])).toBe('public.name');
    });
  });

  describe('when table is null', () => {
    it('returns "Empty Object"', () => {
      expect(getTableDisplayName(null)).toBe('Empty Object');
    });
  });

  describe('when table is undefined', () => {
    it('returns "Empty Object"', () => {
      expect(getTableDisplayName(undefined)).toBe('Empty Object');
    });
  });

  describe('when table is string', () => {
    it('returns the value of table', () => {
      expect(getTableDisplayName('aTable')).toBe('aTable');
    });
  });

  describe('when table is object and includes "name"', () => {
    it('returns .name if object has a schema key (Postgres)', () => {
      expect(getTableDisplayName({ name: 'aName' })).toBe('aName');
    });
    it('returns name and other keys concatenated (non Postgres DBs)', () => {
      expect(getTableDisplayName({ name: 'aName', dataset: 'aDataset' })).toBe(
        'aDataset.aName'
      );
    });
  });

  describe('when table is object without name', () => {
    it('returns the values concatenated', () => {
      expect(
        getTableDisplayName({ collection: 'aCollection', dataset: 'chinook' })
      ).toBe('aCollection.chinook');
    });

    it('returns the values deterministically concatenated', () => {
      expect(
        getTableDisplayName({ dataset: 'chinook', collection: 'aCollection' })
      ).toBe('aCollection.chinook');
    });
  });

  describe('when table is a number', () => {
    it('returns the number as string', () => {
      expect(getTableDisplayName(1)).toBe('1');
    });
  });
});
