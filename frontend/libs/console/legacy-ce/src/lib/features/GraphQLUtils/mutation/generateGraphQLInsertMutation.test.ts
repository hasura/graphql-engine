import { formatSdl } from 'format-graphql';
import { generateGraphQLInsertMutation } from './generateGraphQLInsertMutation';

describe('generateGraphQLInsertMutation for table with', () => {
  it('no table customization + no source customization', () => {
    const result = generateGraphQLInsertMutation({
      defaultQueryRoot: 'Album',
      objects: [
        { AlbumId: 1, ArtistId: 1, Title: 'foo' },
        { AlbumId: 2, ArtistId: 2, Title: 'bar' },
      ],
      mutationName: 'insertAlbumRows',
    });
    const expectedGqlQuery = `
    mutation insertAlbumRows {
      insert_Album(objects: [{AlbumId: 1, ArtistId: 1, Title: "foo"},{AlbumId: 2, ArtistId: 2, Title: "bar"}]) {
        affected_rows
      }
    }
    `;
    expect(result.query).toMatch(formatSdl(expectedGqlQuery));
  });

  it('with table customization (only custom name) + no source customization', () => {
    const result = generateGraphQLInsertMutation({
      defaultQueryRoot: 'Album',
      objects: [
        { AlbumId: 1, ArtistId: 1, Title: 'foo' },
        { AlbumId: 2, ArtistId: 2, Title: 'bar' },
      ],
      mutationName: 'insertAlbumRows',
      tableCustomization: {
        custom_name: 'CustomTableName',
      },
    });
    const expectedGqlQuery = `
    mutation insertAlbumRows {
      insert_CustomTableName(objects: [{AlbumId: 1, ArtistId: 1, Title: "foo"},{AlbumId: 2, ArtistId: 2, Title: "bar"}]) {
        affected_rows
      }
    }
    `;
    expect(result.query).toMatch(formatSdl(expectedGqlQuery));
  });

  it('with table customization (custom insert root) + no source customization', () => {
    const result = generateGraphQLInsertMutation({
      defaultQueryRoot: 'Album',
      objects: [
        { AlbumId: 1, ArtistId: 1, Title: 'foo' },
        { AlbumId: 2, ArtistId: 2, Title: 'bar' },
      ],
      mutationName: 'insertAlbumRows',
      tableCustomization: {
        custom_root_fields: { insert: 'CustomInsertRoot' },
      },
    });
    const expectedGqlQuery = `
    mutation insertAlbumRows {
      CustomInsertRoot(objects: [{AlbumId: 1, ArtistId: 1, Title: "foo"},{AlbumId: 2, ArtistId: 2, Title: "bar"}]) {
        affected_rows
      }
    }
    `;
    expect(result.query).toMatch(formatSdl(expectedGqlQuery));
  });

  it('with table customization (custom name + custom insert root) + no source customization', () => {
    const result = generateGraphQLInsertMutation({
      defaultQueryRoot: 'Album',
      objects: [
        { AlbumId: 1, ArtistId: 1, Title: 'foo' },
        { AlbumId: 2, ArtistId: 2, Title: 'bar' },
      ],
      mutationName: 'insertAlbumRows',
      tableCustomization: {
        custom_name: 'CustomTableName',
        custom_root_fields: { insert: 'CustomInsertRoot' },
      },
    });
    const expectedGqlQuery = `
    mutation insertAlbumRows {
      CustomInsertRoot(objects: [{AlbumId: 1, ArtistId: 1, Title: "foo"},{AlbumId: 2, ArtistId: 2, Title: "bar"}]) {
        affected_rows
      }
    }
    `;
    expect(result.query).toMatch(formatSdl(expectedGqlQuery));
  });

  it('with table customization (custom name + custom insert root) + source customization', () => {
    const result = generateGraphQLInsertMutation({
      defaultQueryRoot: 'Album',
      objects: [
        { AlbumId: 1, ArtistId: 1, Title: 'foo' },
        { AlbumId: 2, ArtistId: 2, Title: 'bar' },
      ],
      mutationName: 'insertAlbumRows',
      tableCustomization: {
        custom_name: 'CustomTableName',
        custom_root_fields: { insert: 'CustomInsertRoot' },
      },
      sourceCustomization: {
        root_fields: {
          namespace: 'MySourceNamespace',
        },
      },
    });
    const expectedGqlQuery = `
    mutation insertAlbumRows {
      MySourceNamespace {
        CustomInsertRoot(objects: [{AlbumId: 1, ArtistId: 1, Title: "foo"}, {AlbumId: 2, ArtistId: 2, Title: "bar"}]) {
          affected_rows
        }
      }
    }
    `;
    expect(result.query).toMatch(formatSdl(expectedGqlQuery));
  });
});
