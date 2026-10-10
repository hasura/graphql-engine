import { generateForeignKeyLabel } from '../utils';

describe('generateForeignKeyLabel', () => {
  it('should accept arrays in target table', () => {
    expect(
      generateForeignKeyLabel({
        from: { columns: ['ArtistId'], table: ['Album'] },
        to: { columns: ['ArtistId'], table: ['Artist'] },
      }),
    ).toBe('ArtistId → Artist.ArtistId');
  });

  it('should accept strings in target table', () => {
    expect(
      generateForeignKeyLabel({
        from: { columns: ['ArtistId'], table: ['Album'] },
        to: { columns: ['ArtistId'], table: 'Artist' },
      }),
    ).toBe('ArtistId → Artist.ArtistId');
  });

  it('should separate nested tables with dots', () => {
    expect(
      generateForeignKeyLabel({
        from: { columns: ['ArtistId'], table: ['Album'] },
        to: { columns: ['ArtistId'], table: ['public', 'Artist'] },
      }),
    ).toBe('ArtistId → public.Artist.ArtistId');
  });

  it('should separate nested columns with commas', () => {
    expect(
      generateForeignKeyLabel({
        from: { columns: ['ArtistId'], table: ['Album'] },
        to: { columns: ['ArtistId', 'AuthorId'], table: ['Artist'] },
      }),
    ).toBe('ArtistId → Artist.ArtistId,AuthorId');
  });

  it('should remove double quotes from columns', () => {
    expect(
      generateForeignKeyLabel({
        from: { columns: ['"ArtistId"'], table: ['Album'] },
        to: { columns: ['"ArtistId"'], table: ['Artist'] },
      }),
    ).toBe('ArtistId → Artist.ArtistId');
  });
});
