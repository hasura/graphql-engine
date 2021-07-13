import { createAsyncThunk, createSlice } from '@reduxjs/toolkit';
import { Driver } from '../../../../../dataSources';
import { ReduxState } from '../../../../../types';
import requestAction from '../../../../../utils/requestAction';
import { AsyncThunkConfig } from '../../../../../store';
import { makeMigrationCall } from '../../DataActions';
import { getRunSqlQuery } from '../../../../Common/utils/v1QueryUtils';
import Endpoints from '../../../../../Endpoints';
import {
  exportMetadataQuery,
  generateReplaceMetadataQuery,
} from '../../../../../metadata/queryUtils';
import { HasuraMetadataV3 } from '../../../../../metadata/types';
import { SchemaSharingFetchingStatus } from './types';
import {
  BASE_URL_PUBLIC,
  BASE_URL_TEMPLATE,
  ROOT_CONFIG_PATH,
} from './schemaSharingConfig';

type SchemaSharingTemplateDetailFull = {
  sql: string;
  longDescription?: string;
  imageUrl?: string;
  publicUrl: string;
  blogPostLink?: string;
  metadataObject?: {
    resource_version: number;
    metadata: HasuraMetadataV3;
  };
  affectedMetadata?: string[]; // TODO defines the possible values
};

export type SchemaSharingTemplateItem = {
  key: string;
  type: 'database';
  title: string;
  description: string;
  relativeFolderPath: string;
  dialect: Driver;
  fetchingStatus: SchemaSharingFetchingStatus;
  isPartialData: boolean;
  details?: SchemaSharingTemplateDetailFull;
};

interface SchemaSharingSection {
  name: string;
  templates: SchemaSharingTemplateItem[];
}

export interface SchemaSharingStore {
  globalConfigState: SchemaSharingFetchingStatus;
  schemas?: {
    sections: SchemaSharingSection[];
  };
}

export interface ServerJsonRootConfig {
  [key: string]: {
    type: 'database';
    dialect: Driver;
    title: string;
    description: string;
    relativeFolderPath: string;
    category: string;
  };
}

export interface ServerJsonSchemaDefinition {
  longDescription?: string;
  imageUrl?: string;
  blogPostLink?: string;
  sqlFiles: string[];
  metadataUrl?: string;
  affectedMetadata?: string[]; // TODO defines the possible values
}

const mapRootJsonFromServerToState = (
  data: ServerJsonRootConfig
): Required<SchemaSharingStore['schemas']> => {
  const sectionsGroups: Record<
    string,
    SchemaSharingTemplateItem[]
  > = Object.entries(data)
    .map(([key, value]) => ({
      ...value,
      key,
    }))
    .reduce<Record<string, SchemaSharingTemplateItem[]>>(
      (previousValue, currentValue) => {
        const item: SchemaSharingTemplateItem = {
          type: 'database',
          isPartialData: true,
          fetchingStatus: 'none',
          key: currentValue.key,
          description: currentValue.description,
          dialect: currentValue.dialect,
          title: currentValue.title,
          relativeFolderPath: currentValue.relativeFolderPath,
        };
        return {
          ...previousValue,
          [currentValue.category]: [
            ...(previousValue[currentValue.category] ?? []),
            item,
          ],
        };
      },
      {}
    );

  const sections: SchemaSharingSection[] = Object.entries(sectionsGroups).map(
    ([name, templates]) => ({
      name,
      templates,
    })
  );

  return {
    sections,
  };
};

const initialStoreState: SchemaSharingStore = {
  globalConfigState: 'none',
  schemas: undefined,
};

export const schemaSharingSelectors = {
  getGlobalConfigState: (state: ReduxState) =>
    state.schemaSharing.globalConfigState,
  getTemplateBySectionAndKey: ({
    key,
    section,
  }: {
    key: string;
    section: string;
  }) => (state: ReduxState) => {
    const maybeSection = state.schemaSharing.schemas?.sections?.find(
      block => block.name === section
    );
    if (!maybeSection) {
      return undefined;
    }
    const maybeTemplate = maybeSection.templates.find(
      template => template.key === key
    );
    if (!maybeTemplate) {
      return undefined;
    }
    return maybeTemplate;
  },
  getSchemasForDb: (driver: Driver) => (state: ReduxState) =>
    state.schemaSharing.schemas?.sections
      .map(section => ({
        ...section,
        templates: section.templates.filter(
          template => template.dialect === driver
        ),
      }))
      .filter(section => section.templates.length > 0),
};

export const fetchGlobalSchemaSharingConfiguration = createAsyncThunk<
  ServerJsonRootConfig,
  undefined,
  AsyncThunkConfig
>('SchemaSharing/GET_REPOSITORY_ROOT_CONFIG', async (params, { dispatch }) => {
  const rawData = await dispatch(requestAction(ROOT_CONFIG_PATH));
  return JSON.parse(rawData);
});

export const fetchSchemaConfigurationByName = createAsyncThunk<
  SchemaSharingTemplateDetailFull,
  { key: string; category: string },
  AsyncThunkConfig
>(
  'SchemaSharing/FETCH_ITEM_CONFIG',
  async ({ key, category }, { dispatch, getState }) => {
    const maybeTemplate = schemaSharingSelectors.getTemplateBySectionAndKey({
      key,
      section: category,
    })(getState());
    if (!maybeTemplate) {
      throw new Error('Template not found');
    }

    const baseTemplatePath = `${BASE_URL_TEMPLATE}/${maybeTemplate.relativeFolderPath}`;
    const publicUrl = `${BASE_URL_PUBLIC}/${maybeTemplate.relativeFolderPath}`;

    const itemConfigRaw = await dispatch(
      requestAction(`${baseTemplatePath}/config.json`)
    );
    const itemConfig: ServerJsonSchemaDefinition = JSON.parse(itemConfigRaw);

    const sqlFiles = await Promise.all(
      itemConfig.sqlFiles.map(sqlFile =>
        dispatch(requestAction<string>(`${baseTemplatePath}/${sqlFile}`))
      )
    );

    const metadataObject = JSON.parse(
      await dispatch(
        requestAction<string>(`${baseTemplatePath}/${itemConfig.metadataUrl}`)
      )
    );

    const fullObject: SchemaSharingTemplateDetailFull = {
      sql: sqlFiles.join('\n'),
      affectedMetadata: itemConfig.affectedMetadata,
      blogPostLink: itemConfig.blogPostLink,
      imageUrl: `${baseTemplatePath}/${itemConfig.imageUrl}`,
      longDescription: itemConfig.longDescription,
      metadataObject,
      publicUrl,
    };

    return fullObject;
  }
);

export const applyTemplate = createAsyncThunk<
  void,
  { key: string; category: string },
  AsyncThunkConfig
>(
  'SchemaSharing/applyTemplate',
  async ({ key, category }, { getState, dispatch }) => {
    let template = schemaSharingSelectors.getTemplateBySectionAndKey({
      key,
      section: category,
    })(getState());
    if (template?.isPartialData) {
      await dispatch(fetchSchemaConfigurationByName({ key, category }));
      template = schemaSharingSelectors.getTemplateBySectionAndKey({
        key,
        section: category,
      })(getState());
    }
    if (!template || !template.details) {
      throw new Error('Template not found');
    }
    const source = getState().tables.currentDataSource;

    const sql = template.details.sql;

    await dispatch(async () => {
      return new Promise((resolve, reject) => {
        makeMigrationCall(
          dispatch,
          getState,
          [getRunSqlQuery(sql ?? '', source)],
          [],
          `apply_sql_template_${key}`,
          resolve,
          reject,
          'Applying sql from template',
          'SQL migration successfully applied',
          'An error occurred while applying the template'
        );
      });
    });

    const { dataHeaders } = getState().tables;

    if (template.details.metadataObject !== undefined) {
      const oldMetadata = await dispatch(
        requestAction<{
          resource_version: number;
          metadata: HasuraMetadataV3;
        }>(Endpoints.metadata, {
          method: 'POST',
          headers: dataHeaders,
          body: JSON.stringify(exportMetadataQuery),
        })
      );

      const newMetadata: HasuraMetadataV3 = {
        ...oldMetadata.metadata,
        sources: oldMetadata.metadata.sources.map(oldSource => {
          if (oldSource.name !== source) {
            return oldSource;
          }
          const metadataObject =
            template?.details?.metadataObject?.metadata?.sources?.[0];
          if (!metadataObject) {
            return oldSource;
          }
          return {
            ...oldSource,
            tables: [...oldSource.tables, ...(metadataObject.tables ?? [])],
            functions: [
              ...(oldSource.functions ?? []),
              ...(metadataObject.functions ?? []),
            ],
          };
        }),
      };

      await dispatch(async () => {
        return new Promise((resolve, reject) => {
          makeMigrationCall(
            dispatch,
            getState,
            [
              generateReplaceMetadataQuery({
                metadata: newMetadata,
                resource_version: 0,
              }),
            ],
            [
              generateReplaceMetadataQuery({
                metadata: newMetadata,
                resource_version: 0,
              }),
            ],
            `apply_metadata_template_${key}`,
            resolve,
            reject,
            'Applying metadata from template',
            `Template ${template?.title} applied`,
            'An error occurred while applying the template'
          );
        });
      });
    }
  }
);

const schemaSharingSlice = createSlice({
  name: 'schemaSharing',
  initialState: initialStoreState,
  reducers: {},
  extraReducers: builder => {
    builder
      .addCase(fetchGlobalSchemaSharingConfiguration.pending, state => {
        state.globalConfigState = 'fetching';
      })
      .addCase(fetchGlobalSchemaSharingConfiguration.rejected, state => {
        state.globalConfigState = 'failure';
      })
      .addCase(
        fetchGlobalSchemaSharingConfiguration.fulfilled,
        (state, { payload }) => {
          state.globalConfigState = 'success';
          state.schemas = mapRootJsonFromServerToState(payload);
        }
      )
      .addCase(
        fetchSchemaConfigurationByName.pending,
        (
          state,
          {
            meta: {
              arg: { category, key },
            },
          }
        ) => {
          const maybeTemplate = state.schemas?.sections
            ?.find?.(section => section.name === category)
            ?.templates?.find(template => template.key === key);
          if (maybeTemplate) {
            maybeTemplate.fetchingStatus = 'fetching';
          }
        }
      )
      .addCase(
        fetchSchemaConfigurationByName.fulfilled,
        (
          state,
          {
            meta: {
              arg: { category, key },
            },
            payload,
          }
        ) => {
          const maybeTemplate = state.schemas?.sections
            ?.find?.(section => section.name === category)
            ?.templates?.find(template => template.key === key);
          if (maybeTemplate) {
            maybeTemplate.fetchingStatus = 'success';
            maybeTemplate.isPartialData = false;
            maybeTemplate.details = payload;
          }
        }
      )
      .addCase(
        fetchSchemaConfigurationByName.rejected,
        (
          state,
          {
            meta: {
              arg: { category, key },
            },
          }
        ) => {
          const maybeTemplate = state.schemas?.sections
            ?.find?.(section => section.name === category)
            ?.templates?.find(template => template.key === key);
          if (maybeTemplate) {
            maybeTemplate.fetchingStatus = 'failure';
          }
        }
      );
  },
});

const { reducer } = schemaSharingSlice;

export const schemaSharingReducer = reducer;
