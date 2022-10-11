import { setupServer } from 'msw/node';
import { configureStore } from '@reduxjs/toolkit';
import {
  applyTemplate,
  fetchGlobalSchemaSharingConfiguration,
  fetchSchemaConfigurationByName,
  templateGalleryReducer,
} from '../Actions';
import { CATEGORY_1, networkStubs } from './stubs/schemaSharingNetworkStubs';
import { TemplateGalleryStore } from '../types';

const server = setupServer();

beforeAll(() => server.listen());
afterEach(() => server.resetHandlers());
afterAll(() => server.close());

const getStore = () => {
  return configureStore<any>({
    reducer: {
      templateGallery: templateGalleryReducer,
      metadata: () => ({ metadataObject: { version: 3 } }),
    },
  });
};

describe('SchemaSharing state', () => {
  describe('global config get flow', () => {
    it('should dispatch the root config success when the api works', async () => {
      server.use(networkStubs.rootJson);

      // any to not add the whole store
      const store = getStore();

      await store.dispatch(fetchGlobalSchemaSharingConfiguration());
      expect(store.getState().templateGallery).toMatchSnapshot();
    });

    it("should dispatch root config failure when the api don't work", async () => {
      server.use(networkStubs.rootJsonError);
      const expected: TemplateGalleryStore = {
        globalConfigState: 'failure',
        templates: undefined,
      };
      // any to not add the whole store
      const store = getStore();

      await store.dispatch(fetchGlobalSchemaSharingConfiguration());

      expect(store.getState().templateGallery).toEqual(expected);
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

      const store = getStore();

      await store.dispatch(fetchGlobalSchemaSharingConfiguration());
      // Act
      await store.dispatch(
        fetchSchemaConfigurationByName({
          key: 'template-1',
          category: CATEGORY_1,
        })
      );

      // Assert

      expect(store.getState().templateGallery).toMatchSnapshot();
    });
    it('should set imageUrl as undefined when there is no image', async () => {
      // Prepare
      server.use(
        networkStubs.rootJson,
        networkStubs.template1.configNoImage,
        networkStubs.template1.firstSql,
        networkStubs.template1.secondSql,
        networkStubs.template1.metadata
      );

      const store = getStore();

      await store.dispatch(fetchGlobalSchemaSharingConfiguration());
      // Act
      await store.dispatch(
        fetchSchemaConfigurationByName({
          key: 'template-1',
          category: CATEGORY_1,
        })
      );

      // Assert

      expect(store.getState().templateGallery).toMatchSnapshot();
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

      const store = getStore();

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
      expect(store.getState().templateGallery).toMatchSnapshot();
    });
  });
  describe('reducer', () => {
    it('should have the correct default state', () => {
      const result = templateGalleryReducer(undefined, { type: '' });

      expect(result).toStrictEqual({
        globalConfigState: 'none',
        templates: undefined,
      });
    });
  });
});
