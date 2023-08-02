import { formatSdl } from 'format-graphql';
import { generateGraphQLDeleteMutation } from './generateGraphQLDeleteMutation';

describe('generateGraphQLDeleteMutation for table with', () => {
  it('single primary key', () => {
    const result = generateGraphQLDeleteMutation({
      defaultQueryRoot: 'Album',
      rows: [{ AlbumId: 1 }, { AlbumId: 2 }],
      mutationName: 'deleteAlbumRows',
    });
    const expectedGqlQuery = `
    mutation deleteAlbumRows {
      delete_Album(where: {_or: [{AlbumId: {_eq: 1}}, {AlbumId: {_eq: 2}}]}) {
        affected_rows
      }
    }
    `;
    expect(result.query).toMatch(formatSdl(expectedGqlQuery));
  });

  it('with composite primary key', () => {
    const result = generateGraphQLDeleteMutation({
      defaultQueryRoot: 'Voting',
      rows: [
        { questionid: 1, memberid: 1 },
        { questionid: 1, memberid: 2 },
      ],
      mutationName: 'deleteAlbumRows',
    });
    const expectedGqlQuery = `
    mutation deleteAlbumRows {
      delete_Voting(
        where: {_or: [{questionid: {_eq: 1}, memberid: {_eq: 1}}, {questionid: {_eq: 1}, memberid: {_eq: 2}}]}
      ) {
        affected_rows
      }
    }
    `;
    expect(result.query).toMatch(formatSdl(expectedGqlQuery));
  });

  it('with a full row match', () => {
    const result = generateGraphQLDeleteMutation({
      defaultQueryRoot: 'Album',
      rows: [{ AlbumId: 4, Title: 'Let There Be Rock', ArtistId: 1 }],
      mutationName: 'deleteAlbumRows',
    });
    const expectedGqlQuery = `
    mutation deleteAlbumRows {
      delete_Album(
        where: {_or: [{AlbumId: {_eq: 4}, Title: {_eq: "Let There Be Rock"}, ArtistId: {_eq: 1}}]}
      ) {
        affected_rows
      }
    }
    `;
    expect(result.query).toMatch(formatSdl(expectedGqlQuery));
  });

  it('with custom table name', () => {
    const result = generateGraphQLDeleteMutation({
      defaultQueryRoot: 'Album',
      rows: [{ AlbumId: 1 }, { AlbumId: 2 }],
      mutationName: 'deleteAlbumRows',
      tableCustomization: {
        custom_name: 'CustomTableName',
      },
    });
    const expectedGqlQuery = `
    mutation deleteAlbumRows {
      delete_CustomTableName(where: {_or: [{AlbumId: {_eq: 1}}, {AlbumId: {_eq: 2}}]}) {
        affected_rows
      }
    }
    `;
    expect(result.query).toMatch(formatSdl(expectedGqlQuery));
  });

  it('with custom delete mutation root', () => {
    const result = generateGraphQLDeleteMutation({
      defaultQueryRoot: 'Album',
      rows: [{ AlbumId: 1 }, { AlbumId: 2 }],
      mutationName: 'deleteAlbumRows',
      tableCustomization: {
        custom_root_fields: {
          delete: 'CustomDeleteRoot',
        },
      },
    });
    const expectedGqlQuery = `
    mutation deleteAlbumRows {
      CustomDeleteRoot(where: {_or: [{AlbumId: {_eq: 1}}, {AlbumId: {_eq: 2}}]}) {
        affected_rows
      }
    }
    `;
    expect(result.query).toMatch(formatSdl(expectedGqlQuery));
  });

  it('with custom delete mutation root + source namespace', () => {
    const result = generateGraphQLDeleteMutation({
      defaultQueryRoot: 'Album',
      rows: [{ AlbumId: 1 }, { AlbumId: 2 }],
      mutationName: 'deleteAlbumRows',
      tableCustomization: {
        custom_root_fields: {
          delete: 'CustomDeleteRoot',
        },
      },
      sourceCustomization: {
        root_fields: {
          namespace: 'MySourceNamespace',
        },
      },
    });
    const expectedGqlQuery = `
    mutation deleteAlbumRows {
      MySourceNamespace {
        CustomDeleteRoot(where: {_or: [{AlbumId: {_eq: 1}}, {AlbumId: {_eq: 2}}]}) {
          affected_rows
        }
      }
    }
    `;
    expect(result.query).toMatch(formatSdl(expectedGqlQuery));
  });
});
