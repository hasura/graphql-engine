import { generateForeignKeyLabel } from '../utils';

describe('generateForeignKeyLabel', () => {
  it('should accept arrays in target table', () => {
    expect(
      generateForeignKeyLabel({
        from: { column: ['ArtistId'], table: ['Album'] },
        to: { column: ['ArtistId'], table: ['Artist'] },
      })
    ).toBe('ArtistId → Artist.ArtistId');
  });

  it('should accept strings in target table', () => {
    expect(
      generateForeignKeyLabel({
        from: { column: ['ArtistId'], table: ['Album'] },
        to: { column: ['ArtistId'], table: 'Artist' },
      })
    ).toBe('ArtistId → public.Artist.ArtistId');
  });

  it('should separate nested tables with dots', () => {
    expect(
      generateForeignKeyLabel({
        from: { column: ['ArtistId'], table: ['Album'] },
        to: { column: ['ArtistId'], table: ['public', 'Artist'] },
      })
    ).toBe('ArtistId → public.Artist.ArtistId');
  });

  it('should separate nested columns with commas', () => {
    expect(
      generateForeignKeyLabel({
        from: { column: ['ArtistId'], table: ['Album'] },
        to: { column: ['ArtistId', 'AuthorId'], table: ['Artist'] },
      })
    ).toBe('ArtistId → Artist.ArtistId,AuthorId');
  });

  it('should remove double quotes from columns', () => {
    expect(
      generateForeignKeyLabel({
        from: { column: ['"ArtistId"'], table: ['Album'] },
        to: { column: ['"ArtistId"'], table: ['Artist'] },
      })
    ).toBe('ArtistId → Artist.ArtistId');
  });
});
