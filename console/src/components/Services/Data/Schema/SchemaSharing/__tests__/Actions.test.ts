import { setupServer } from 'msw/node';
import { configureStore } from '@reduxjs/toolkit';
import {
  applyTemplate,
  fetchGlobalSchemaSharingConfiguration,
  fetchSchemaConfigurationByName,
  schemaSharingReducer,
  SchemaSharingStore,
} from '../Actions';
import { CATEGORY_1, networkStubs } from './stubs/schemaSharingNetworkStubs';

const server = setupServer();

beforeAll(() => server.listen());
afterEach(() => server.resetHandlers());
afterAll(() => server.close());

describe('SchemaSharing state', () => {
  describe('global config get flow', () => {
    it('should dispatch the root config success when the api works', async () => {
      server.use(networkStubs.rootJson);

      // any to not add the whole store
      const store = configureStore<any>({
        reducer: {
          schemaSharing: schemaSharingReducer,
        },
      });

      await store.dispatch(fetchGlobalSchemaSharingConfiguration());
      expect(store.getState().schemaSharing).toMatchSnapshot();
    });

    it("should dispatch root config failure when the api don't work", async () => {
      server.use(networkStubs.rootJsonError);
      const expected: SchemaSharingStore = {
        globalConfigState: 'failure',
        schemas: undefined,
      };
      // any to not add the whole store
      const store = configureStore<any>({
        reducer: {
          schemaSharing: schemaSharingReducer,
        },
      });

      await store.dispatch(fetchGlobalSchemaSharingConfiguration());

      expect(store.getState()).toEqual({
        schemaSharing: expected,
      });
    });
  });
  describe('fetch single config file', () => {
    it('should populate with all the data from the gihtub api', async () => {
      // Prepare
      server.use(
        networkStubs.rootJson,
        networkStubs.template1.config,
        networkStubs.template1.firstSql,
        networkStubs.template1.secondSql,
        networkStubs.template1.metadata
      );

      const store = configureStore<any>({
        reducer: {
          schemaSharing: schemaSharingReducer,
        },
      });
      await store.dispatch(fetchGlobalSchemaSharingConfiguration());
      // Act
      await store.dispatch(
        fetchSchemaConfigurationByName({
          key: 'template-1',
          category: CATEGORY_1,
        })
      );

      // Assert

      expect(store.getState().schemaSharing).toMatchSnapshot();
    });
  });
  describe('apply schema', () => {
    it('should apply migration', async () => {
      // Prepare
      server.use(
        networkStubs.rootJson,
        networkStubs.template1.config,
        networkStubs.template1.firstSql,
        networkStubs.template1.secondSql,
        networkStubs.template1.metadata
      );

      const store = configureStore<any>({
        reducer: {
          schemaSharing: schemaSharingReducer,
        },
      });
      await store.dispatch(fetchGlobalSchemaSharingConfiguration());
      await store.dispatch(
        fetchSchemaConfigurationByName({
          key: 'template-1',
          category: CATEGORY_1,
        })
      );

      // Act
      await store.dispatch(
        applyTemplate({
          key: 'template-1',
          category: CATEGORY_1,
        })
      );

      // Assert
      expect(store.getState().schemaSharing).toMatchSnapshot();
    });
  });
  describe('reducer', () => {
    it('should have the correct default state', () => {
      const result = schemaSharingReducer(undefined, { type: '' });

      expect(result).toStrictEqual({
        globalConfigState: 'none',
        schemas: undefined,
      });
    });
  });
});
