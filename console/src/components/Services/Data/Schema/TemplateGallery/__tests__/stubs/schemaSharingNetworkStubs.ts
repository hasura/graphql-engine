import { rest } from 'msw';
import { HasuraMetadataV3 } from '../../../../../../../metadata/types';
import {
  BASE_URL_TEMPLATE,
  ROOT_CONFIG_PATH,
} from '../../templateGalleryConfig';
import { ServerJsonRootConfig } from '../../types';

export const CATEGORY_1 = 'AAAAAAAA';
export const CATEGORY_2 = 'BBBBBBBB';

export const MOCK_ROOT_CONFIG: ServerJsonRootConfig = {
  'template-1': {
    template_version: '1',
    metadata_version: '3',
    title: 'template-1',
    relativeFolderPath: './postgres-template-1',
    dialect: 'postgres',
    description: 'This is the description of template one',
    type: 'database',
    category: CATEGORY_1,
  },
  'template-2': {
    template_version: '1',
    metadata_version: '3',
    title: 'template-2',
    relativeFolderPath: '/home',
    dialect: 'postgres',
    description: 'This is the description of template two',
    type: 'database',
    category: CATEGORY_2,
  },
  'template-3': {
    template_version: '1',
    metadata_version: '3',
    title: 'template-3',
    relativeFolderPath: '/home',
    dialect: 'mysql',
    description: 'This is the description of template 3',
    type: 'database',
    category: CATEGORY_2,
  },
  'template-3-ignore-template-version': {
    template_version: '2',
    metadata_version: '3',
    title: 'template-3-ignore',
    relativeFolderPath: '/home',
    dialect: 'mysql',
    description: 'This is the description of template 3',
    type: 'database',
    category: CATEGORY_2,
  },
  'template-3-ignore-metadata': {
    template_version: '1',
    metadata_version: '5',
    title: 'template-3-ignore',
    relativeFolderPath: '/home',
    dialect: 'mysql',
    description: 'This is the description of template 3',
    type: 'database',
    category: CATEGORY_2,
  },
};

export const CURRENT_MOCK_METAe: HasuraMetadataV3 = {
  version: 3,
  inherited_roles: [],
  actions: [],
  allowlist: [],
  cron_triggers: [],
  query_collections: [],
  remote_schemas: [],
  rest_endpoints: [],
  sources: [
    {
      name: 'default',
      kind: 'postgres',
      tables: [
        {
          table: {
            schema: '_onetoone',
            name: 'owner',
          },
        },
      ],
      configuration: {
        connection_info: {
          use_prepared_statements: true,
          database_url: {
            from_env: 'HASURA_GRAPHQL_DATABASE_URL',
          },
          isolation_level: 'read-committed',
          pool_settings: {
            connection_lifetime: 600,
            retries: 1,
            idle_timeout: 180,
            max_connections: 50,
          },
        },
      },
    },
  ],
};

export const MOCK_ONE_TO_ONE_METADATA = {
  resource_version: 13,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'default',
        kind: 'postgres',
        tables: [
          {
            table: {
              schema: '_onetoone',
              name: 'owner',
            },
            object_relationships: [
              {
                name: 'passport_info',
                using: {
                  foreign_key_constraint_on: {
                    column: 'owner_id',
                    table: {
                      schema: '_onetoone',
                      name: 'passport_info',
                    },
                  },
                },
              },
            ],
          },
          {
            table: {
              schema: '_onetoone',
              name: 'passport_info',
            },
            object_relationships: [
              {
                name: 'owner',
                using: {
                  foreign_key_constraint_on: 'owner_id',
                },
              },
            ],
          },
        ],
        configuration: {
          connection_info: {
            use_prepared_statements: true,
            database_url: {
              from_env: 'HASURA_GRAPHQL_DATABASE_URL_OTHER',
            },
            isolation_level: 'read-committed',
            pool_settings: {
              connection_lifetime: 6000,
              retries: 10,
              idle_timeout: 1800,
              max_connections: 500,
            },
          },
        },
      },
    ],
  },
};
const BASE_PS_URL = `${BASE_URL_TEMPLATE}/./postgres-template-1`;

export const networkStubs = {
  rootJson: rest.get(ROOT_CONFIG_PATH, (req, res, context) => {
    return res(context.text(JSON.stringify(MOCK_ROOT_CONFIG)));
  }),
  rootJsonEmpty: rest.get(ROOT_CONFIG_PATH, (req, res, context) => {
    return res(context.text(JSON.stringify({})));
  }),
  rootJsonWithLoading: rest.get(ROOT_CONFIG_PATH, (req, res, context) => {
    return res(
      context.text(JSON.stringify(MOCK_ROOT_CONFIG)),
      context.delay(1000)
    );
  }),
  rootJsonError: rest.get(ROOT_CONFIG_PATH, (req, res, context) => {
    return res(context.status(500), context.json({ message: 'ERROR' }));
  }),
  template1: {
    config: rest.get(`${BASE_PS_URL}/config.json`, (req, res, context) =>
      res(
        context.text(
          JSON.stringify({
            longDescription: 'long long description',
            imageUrl: 'someImage.url',
            blogPostLink:
              'https://hasura.io/blog/whats-new-in-hasura-cloud-may-2021/',
            sqlFiles: ['./first.sql', './second.sql'],
            metadataUrl: './meta.json',
            affectedMetadata: ['some', 'meta'],
          })
        )
      )
    ),
    configNoImage: rest.get(`${BASE_PS_URL}/config.json`, (req, res, context) =>
      res(
        context.text(
          JSON.stringify({
            longDescription: 'long long description',
            blogPostLink:
              'https://hasura.io/blog/whats-new-in-hasura-cloud-may-2021/',
            sqlFiles: ['./first.sql', './second.sql'],
            metadataUrl: './meta.json',
            affectedMetadata: ['some', 'meta'],
          })
        )
      )
    ),
    configSlow: rest.get(`${BASE_PS_URL}/config.json`, (req, res, context) =>
      res(
        context.text(
          JSON.stringify({
            longDescription: 'long long description',
            imageUrl: 'someImage.url',
            blogPostLink:
              'https://hasura.io/blog/whats-new-in-hasura-cloud-may-2021/',
            sqlFiles: ['./first.sql', './second.sql'],
            metadataUrl: './meta.json',
            affectedMetadata: ['some', 'meta'],
          })
        ),
        context.delay(1000)
      )
    ),
    firstSql: rest.get(`${BASE_PS_URL}/first.sql`, (req, res, context) =>
      res(
        context.text(`
CREATE SCHEMA _onetoone;

-- Create Tables
CREATE TABLE _onetoone.owner (
    id serial PRIMARY KEY,
    name text NOT NULL
);

CREATE TABLE _onetoone.passport_info (
    id serial PRIMARY KEY,
    passport_number text NOT NULL UNIQUE,
    owner_id integer REFERENCES _onetoone.owner(id) NOT NULL
);`)
      )
    ),
    secondSql: rest.get(`${BASE_PS_URL}/second.sql`, (req, res, context) =>
      res(
        context.text(`
INSERT INTO "_onetoone"."owner" ("name") VALUES
('Coleman Spickett'),
('Gallard Dreye'),;

INSERT INTO "_onetoone"."passport_info" ("passport_number", "owner_id") VALUES
('553221', 1),
('839016', 2),`)
      )
    ),
    secondSqlError: rest.get(`${BASE_PS_URL}/second.sql`, (req, res, context) =>
      res(context.text('Not found'), context.status(404))
    ),
    metadata: rest.get(`${BASE_PS_URL}/meta.json`, (req, res, context) =>
      res(context.text(JSON.stringify(MOCK_ONE_TO_ONE_METADATA)))
    ),
  },
};
