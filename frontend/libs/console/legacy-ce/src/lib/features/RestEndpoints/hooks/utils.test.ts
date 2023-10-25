import { formatSdl } from 'format-graphql';
import { Microfiber } from 'microfiber';
import introspectionNoCustom from './mocks/introspectionWithoutCustomizations.json';
import introspectionCustom from './mocks/introspectionWithCustomizations.json';

import {
  generateDeleteEndpoint,
  generateInsertEndpoint,
  generateUpdateEndpoint,
  generateViewAllEndpoint,
  generateViewEndpoint,
} from './utils';
import { getOperations } from './useRestEndpointDefinitions';

const microfiberNoCustom = new Microfiber(introspectionNoCustom);
const operationsWithoutCustom = getOperations('', microfiberNoCustom);
const microfiberCustom = new Microfiber(introspectionCustom);
const operationsCustom = getOperations('root_', microfiberCustom);

describe('generateViewEndpoint', () => {
  it('should generate a query and a rest endpoint for a view operation without customizations', () => {
    const table = 'user';

    const { query, restEndpoint } = generateViewEndpoint(
      '',
      table,
      operationsWithoutCustom?.find(({ name }) => name === 'user_by_pk'),
      microfiberNoCustom
    );

    expect(query).toEqual({
      name: 'user_by_pk',
      query: formatSdl(`query user_by_pk($id: Int!) {
      user_by_pk(id: $id) {
        address
        bool
        count
        date
        id
        name
        uuid
      }
    }`),
    });
    expect(restEndpoint).toEqual({
      name: 'user_by_pk',
      url: `${table}/:id`,
      methods: ['GET'],
      definition: {
        query: {
          query_name: 'user_by_pk',
          collection_name: 'allowed-queries',
        },
      },
      comment: '',
    });
  });

  it('should generate a query and a rest endpoint for a view operation with customizations', () => {
    const table = 'user';

    const { query, restEndpoint } = generateViewEndpoint(
      'root_',
      table,
      operationsCustom?.find(({ name }) => name === 'a_user_by_pk_b'),
      microfiberCustom
    );

    expect(query).toEqual({
      name: 'a_user_by_pk_b',
      query: formatSdl(`query a_user_by_pk_b($id: Int!) {
      root_ {
        a_user_by_pk_b(id: $id) {
          address
          bool
          count
          date
          id
          name
          uuid
        }
      }
    }`),
    });
    expect(restEndpoint).toEqual({
      name: 'a_user_by_pk_b',
      url: `${table}/:id`,
      methods: ['GET'],
      definition: {
        query: {
          query_name: 'a_user_by_pk_b',
          collection_name: 'allowed-queries',
        },
      },
      comment: '',
    });
  });
});

describe('generateViewAllEndpoint', () => {
  it('should generate a query and a rest endpoint for a view all operation without customizations', () => {
    const table = 'user';

    const { query, restEndpoint } = generateViewAllEndpoint(
      '',
      table,
      operationsWithoutCustom?.find(({ name }) => name === 'user'),
      microfiberNoCustom
    );

    expect(query).toEqual({
      name: 'user',
      query: formatSdl(`query user {
      user {
        address
        bool
        count
        date
        id
        name
        uuid
      }
    }`),
    });
    expect(restEndpoint).toEqual({
      name: 'user',
      url: `${table}`,
      methods: ['GET'],
      definition: {
        query: {
          query_name: 'user',
          collection_name: 'allowed-queries',
        },
      },
      comment: '',
    });
  });

  it('should generate a query and a rest endpoint for a view all operation with customizations', () => {
    const table = 'user';

    const { query, restEndpoint } = generateViewAllEndpoint(
      'root_',
      table,
      operationsCustom?.find(({ name }) => name === 'a_user_b'),
      microfiberCustom
    );

    expect(query).toEqual({
      name: 'a_user_b',
      query: formatSdl(`query a_user_b {
      root_ {
        a_user_b {
          address
          bool
          count
          date
          id
          name
          uuid
        }
      }
    }`),
    });
    expect(restEndpoint).toEqual({
      name: 'a_user_b',
      url: `${table}`,
      methods: ['GET'],
      definition: {
        query: {
          query_name: 'a_user_b',
          collection_name: 'allowed-queries',
        },
      },
      comment: '',
    });
  });
});

describe('generateDeleteEndpoint', () => {
  it('should generate a query and a rest endpoint for a delete operation without customizations', () => {
    const table = 'user';

    const { query, restEndpoint } = generateDeleteEndpoint(
      '',
      table,
      operationsWithoutCustom?.find(({ name }) => name === 'delete_user_by_pk'),
      microfiberNoCustom
    );

    expect(query).toEqual({
      name: 'delete_user_by_pk',
      query: formatSdl(`mutation delete_user_by_pk($id: Int!) {
      delete_user_by_pk(id: $id) {
        address
        bool
        count
        date
        id
        name
        uuid
      }
    }`),
    });
    expect(restEndpoint).toEqual({
      name: 'delete_user_by_pk',
      url: `${table}/:id`,
      methods: ['DELETE'],
      definition: {
        query: {
          query_name: 'delete_user_by_pk',
          collection_name: 'allowed-queries',
        },
      },
      comment: '',
    });
  });

  it('should generate a query and a rest endpoint for a delete operation with customizations', () => {
    const table = 'user';

    const { query, restEndpoint } = generateDeleteEndpoint(
      'root_',
      table,
      operationsCustom?.find(({ name }) => name === 'a_delete_user_by_pk_b'),
      microfiberCustom
    );

    expect(query).toEqual({
      name: 'a_delete_user_by_pk_b',
      query: formatSdl(`mutation a_delete_user_by_pk_b($id: Int!) {
      root_ {
        a_delete_user_by_pk_b(id: $id) {
          address
          bool
          count
          date
          id
          name
          uuid
        }
      }
    }`),
    });
    expect(restEndpoint).toEqual({
      name: 'a_delete_user_by_pk_b',
      url: `${table}/:id`,
      methods: ['DELETE'],
      definition: {
        query: {
          query_name: 'a_delete_user_by_pk_b',
          collection_name: 'allowed-queries',
        },
      },
      comment: '',
    });
  });
});

describe('generateUpdateEndpoint', () => {
  it('should generate a query and a rest endpoint for an update operation without customizations', () => {
    const table = 'user';

    const { query, restEndpoint } = generateUpdateEndpoint(
      '',
      table,
      operationsWithoutCustom?.find(({ name }) => name === 'update_user_by_pk'),
      microfiberNoCustom
    );

    expect(query).toEqual({
      name: 'update_user_by_pk',
      query:
        formatSdl(`mutation update_user_by_pk($id: Int!, $object: user_set_input!) {
      update_user_by_pk(pk_columns: {id: $id}, _set: $object) {
        address
        bool
        count
        date
        id
        name
        uuid
      }
    }`),
    });
    expect(restEndpoint).toEqual({
      name: 'update_user_by_pk',
      url: `${table}/:id`,
      methods: ['POST'],
      definition: {
        query: {
          query_name: 'update_user_by_pk',
          collection_name: 'allowed-queries',
        },
      },
      comment: '',
    });
  });

  it('should generate a query and a rest endpoint for an update operation with customizations', () => {
    const table = 'user';

    const { query, restEndpoint } = generateUpdateEndpoint(
      'root_',
      table,
      operationsCustom?.find(({ name }) => name === 'a_update_user_by_pk_b'),
      microfiberCustom
    );

    expect(query).toEqual({
      name: 'a_update_user_by_pk_b',
      query:
        formatSdl(`mutation a_update_user_by_pk_b($id: Int!, $object: c_user_set_input_d!) {
      root_ {
        a_update_user_by_pk_b(pk_columns: {id: $id}, _set: $object) {
          address
          bool
          count
          date
          id
          name
          uuid
        }
      }
    }`),
    });
    expect(restEndpoint).toEqual({
      name: 'a_update_user_by_pk_b',
      url: `${table}/:id`,
      methods: ['POST'],
      definition: {
        query: {
          query_name: 'a_update_user_by_pk_b',
          collection_name: 'allowed-queries',
        },
      },
      comment: '',
    });
  });
});

describe('generateInsertEndpoint', () => {
  it('should generate a query and a rest endpoint for an insert operation without customizations', () => {
    const table = 'user';

    const { query, restEndpoint } = generateInsertEndpoint(
      '',
      table,
      operationsWithoutCustom?.find(({ name }) => name === 'insert_user_one'),
      microfiberNoCustom
    );

    expect(query).toEqual({
      name: 'insert_user_one',
      query: formatSdl(`mutation insert_user_one($object: user_insert_input!) {
      insert_user_one(object: $object) {
        address
        bool
        count
        date
        id
        name
        uuid
      }
    }`),
    });
    expect(restEndpoint).toEqual({
      name: 'insert_user_one',
      url: `${table}`,
      methods: ['POST'],
      definition: {
        query: {
          query_name: 'insert_user_one',
          collection_name: 'allowed-queries',
        },
      },
      comment: '',
    });
  });

  it('should generate a query and a rest endpoint for an insert operation with customizations', () => {
    const table = 'user';

    const { query, restEndpoint } = generateInsertEndpoint(
      'root_',
      table,
      operationsCustom?.find(({ name }) => name === 'a_insert_user_one_b'),
      microfiberCustom
    );

    expect(query).toEqual({
      name: 'a_insert_user_one_b',
      query:
        formatSdl(`mutation a_insert_user_one_b($object: c_user_insert_input_d!) {
      root_ {
        a_insert_user_one_b(object: $object) {
          address
          bool
          count
          date
          id
          name
          uuid
        }
      }
    }`),
    });
    expect(restEndpoint).toEqual({
      name: 'a_insert_user_one_b',
      url: `${table}`,
      methods: ['POST'],
      definition: {
        query: {
          query_name: 'a_insert_user_one_b',
          collection_name: 'allowed-queries',
        },
      },
      comment: '',
    });
  });
});
