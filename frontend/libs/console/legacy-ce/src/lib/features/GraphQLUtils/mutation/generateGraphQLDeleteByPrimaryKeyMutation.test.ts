import { formatSdl } from 'format-graphql';
import { generateGraphQLDeleteByPrimaryKeyMutation } from './generateGraphQLDeleteByPrimaryKeyMutation';

describe('generateGraphQLDeleteByPrimaryKeyMutation for table with', () => {
  it('single primary key', () => {
    const result = generateGraphQLDeleteByPrimaryKeyMutation({
      defaultQueryRoot: 'Album',
      row: { AlbumId: 1 },
      primaryKeys: ['AlbumId'],
      mutationName: 'deleteAlbumRows',
    });
    const expectedGqlQuery = `
    mutation deleteAlbumRows {
      delete_Album_by_pk(AlbumId: 1) {
        AlbumId
      }
    }
    `;
    expect(result.query).toMatch(formatSdl(expectedGqlQuery));
  });

  it('with composite primary key', () => {
    const result = generateGraphQLDeleteByPrimaryKeyMutation({
      defaultQueryRoot: 'Voting',
      row: { questionid: 1, memberid: 1 },
      primaryKeys: ['questionid', 'memberid'],
      mutationName: 'deleteAlbumRows',
    });
    const expectedGqlQuery = `
    mutation deleteAlbumRows {
      delete_Voting_by_pk(questionid: 1, memberid: 1) {
        questionid
        memberid
      }
    }
    `;
    expect(result.query).toMatch(formatSdl(expectedGqlQuery));
  });

  it('with custom table name', () => {
    const result = generateGraphQLDeleteByPrimaryKeyMutation({
      defaultQueryRoot: 'Album',
      row: { AlbumId: 1 },
      primaryKeys: ['AlbumId'],
      mutationName: 'deleteAlbumRows',
      tableCustomization: {
        custom_name: 'CustomTableName',
      },
    });
    const expectedGqlQuery = `
    mutation deleteAlbumRows {
      delete_CustomTableName_by_pk(AlbumId: 1) {
        AlbumId
      }
    }
    `;
    expect(result.query).toMatch(formatSdl(expectedGqlQuery));
  });

  it('with custom delete mutation root', () => {
    const result = generateGraphQLDeleteByPrimaryKeyMutation({
      defaultQueryRoot: 'Album',
      row: { AlbumId: 1 },
      primaryKeys: ['AlbumId'],
      mutationName: 'deleteAlbumRows',
      tableCustomization: {
        custom_root_fields: {
          delete_by_pk: 'CustomDeleteRoot',
        },
      },
    });
    const expectedGqlQuery = `
    mutation deleteAlbumRows {
      CustomDeleteRoot(AlbumId: 1) {
        AlbumId
      }
    }
    `;
    expect(result.query).toMatch(formatSdl(expectedGqlQuery));
  });

  it('with custom delete mutation root + source namespace', () => {
    const result = generateGraphQLDeleteByPrimaryKeyMutation({
      defaultQueryRoot: 'Album',
      row: { AlbumId: 1 },
      primaryKeys: ['AlbumId'],
      mutationName: 'deleteAlbumRows',
      tableCustomization: {
        custom_root_fields: {
          delete_by_pk: 'CustomDeleteRoot',
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
        CustomDeleteRoot(AlbumId: 1) {
          AlbumId
        }
      }
    }
    `;
    expect(result.query).toMatch(formatSdl(expectedGqlQuery));
  });
});
