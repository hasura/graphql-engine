import {
  FetchAllSurveysDataQuery,
  Survey_V2_Question_Kind_Enum,
} from '../../../ControlPlane';
import {
  getMetadataUrl,
  getMigrationUrl,
  getSampleQueriesUrl,
  getSchemaImageUrl,
  NEON_TEMPLATE_BASE_PATH,
} from '../../constants';
import { OnboardingResponseData } from '../../types';

const userMock = {
  id: '59300b64-fb3a-4f17-8a0f-6f698569eade',
  created_at: '2022-11-02T13:31:32.526653286Z',
};

export const mockOnboardingData: Record<
  string,
  OnboardingResponseData['data']
> = {
  /**
   *  config to show wizard as user activity is empty
   */
  emptyActivity: {
    user_onboarding: [
      {
        activity: {},
        target: 'cloud_console',
      },
    ],
    users: [userMock],
    one_click_deployment: [],
    one_click_deployment_sample_apps: [],
  },
  /**
   *  config to hide wizard as hasura data source created
   */
  hasuraDataSourceCreationStart: {
    user_onboarding: [
      {
        activity: {
          project_id: {
            value: '44300b64-fb3a-4f17-8a0f-6f698568eade',
            version: 'v1',
          },
          run_query_skip: {},
          run_query_click: {},
          neon_login_start: {},
          skipped_onboarding: {},
          onboarding_complete: {},
          neon_onboarding_error: {},
          install_template_start: {
            value: 'true',
            version: 'v1',
            action_time: '2022-11-02T13:31:32.526653286Z',
          },
          neon_db_creation_start: {
            value: 'true',
            version: 'v1',
            action_time: '2022-11-02T13:31:18.114022923Z',
          },
          hasura_source_creation_start: {
            value: 'true',
            version: 'v1',
            action_time: '2022-11-02T13:31:32.492540278Z',
          },
        },
        target: 'cloud_console',
      },
    ],
    users: [userMock],
    one_click_deployment: [],
    one_click_deployment_sample_apps: [],
  },
  /**
   *  config to hide wizard as run query clicked
   */
  runQueryClick: {
    user_onboarding: [
      {
        activity: {
          project_id: {
            value: '44300b64-fb3a-4f17-8a0f-6f698568eade',
            version: 'v1',
          },
          run_query_skip: {},
          run_query_click: {
            value: 'true',
            version: 'v1',
            action_time: '2022-11-02T13:32:54.084118133Z',
          },
          neon_login_start: {},
          skipped_onboarding: {},
          onboarding_complete: {},
          neon_onboarding_error: {},
          install_template_start: {
            value: 'true',
            version: 'v1',
            action_time: '2022-11-02T13:31:32.526653286Z',
          },
          neon_db_creation_start: {
            value: 'true',
            version: 'v1',
            action_time: '2022-11-02T13:31:18.114022923Z',
          },
          hasura_source_creation_start: {
            value: 'true',
            version: 'v1',
            action_time: '2022-11-02T13:31:32.492540278Z',
          },
        },
        target: 'cloud_console',
      },
    ],
    users: [userMock],
    one_click_deployment: [],
    one_click_deployment_sample_apps: [],
  },
  /**
   *  config to hide wizard as user skipped onboarding
   */
  skippedOnboarding: {
    user_onboarding: [
      {
        activity: {
          project_id: {
            value: '44300b64-fb3a-4f17-8a0f-6f698568eade',
            version: 'v1',
          },
          run_query_skip: {},
          run_query_click: {},
          neon_login_start: {},
          skipped_onboarding: {
            value: 'true',
            version: 'v1',
            action_time: '2022-11-02T14:49:49.908295178Z',
          },
          onboarding_complete: {},
          neon_onboarding_error: {},
          install_template_start: {},
          neon_db_creation_start: {},
          hasura_source_creation_start: {},
        },
        target: 'cloud_console',
      },
    ],
    users: [userMock],
    one_click_deployment: [],
    one_click_deployment_sample_apps: [],
  },
  /**
   *  config to hide wizard as user completed onboarding
   */
  completedOnboarding: {
    user_onboarding: [
      {
        activity: {
          project_id: {
            value: '44300b64-fb3a-4f17-8a0f-6f698568eade',
            version: 'v1',
          },
          run_query_skip: {},
          run_query_click: {},
          neon_login_start: {},
          skipped_onboarding: {},
          onboarding_complete: {
            value: 'true',
            version: 'v1',
            action_time: '2022-11-02T14:49:49.908295178Z',
          },
          neon_onboarding_error: {},
          install_template_start: {},
          neon_db_creation_start: {},
          hasura_source_creation_start: {},
        },
        target: 'cloud_console',
      },
    ],
    users: [userMock],
    one_click_deployment: [],
    one_click_deployment_sample_apps: [],
  },
};

export const fetchSurveysDataResponse: Record<
  string,
  FetchAllSurveysDataQuery
> = {
  unanswered: {
    survey_v2: [
      {
        survey_name: 'Hasura familiarity survey',
        survey_title: null,
        survey_description: null,
        survey_questions: [
          {
            id: '57bff905-4b33-4195-8091-3ecd30db87b0',
            position: 1,
            is_mandatory: true,
            question: 'How familiar are you with Hasura?',
            kind: 'radio' as Survey_V2_Question_Kind_Enum,
            survey_question_options: [
              {
                id: '05b532b4-2e2a-49a3-8ade-215107c4d64f',
                position: 1,
                option: "I'm completely new to Hasura",
                template_config:
                  '{ "react_icons_fa_component_name": "FaHeart" }',
                additional_info_config: null,
              },
              {
                id: 'd6cd3f20-5add-4283-88c3-d112b7446369',
                position: 1,
                option:
                  "I've used Hasura before but not actively developing right now",
                template_config:
                  '{ "react_icons_fa_component_name": "FaBookmark" }',
                additional_info_config: null,
              },
              {
                id: '76c30481-a7cc-45b4-9bd0-7332a2fb8617',
                position: 1,
                option: "I'm already using Hasura (CE/Cloud) weekly/monthly",
                template_config:
                  '{ "react_icons_fa_component_name": "FaUser" }',
                additional_info_config: null,
              },
              {
                id: '319bb5c4-9aa6-42eb-afce-3862c4bef3a7',
                position: 1,
                option: "I'm actively developing with Hasura (CE/Cloud) daily",
                template_config:
                  '{ "react_icons_fa_component_name": "FaStar" }',
                additional_info_config: null,
              },
            ],
          },
        ],
        survey_responses: [],
        template_config:
          '{ "survey_submit_text": "Submit", "survey_cancel_text": "Cancel" }',
      },
    ],
  },
  answered: {
    survey_v2: [
      {
        survey_name: 'Hasura familiarity survey',
        survey_title: null,
        survey_description: null,
        survey_questions: [
          {
            id: '57bff905-4b33-4195-8091-3ecd30db87b0',
            position: 1,
            is_mandatory: true,
            question: 'How familiar are you with Hasura?',
            kind: 'radio' as Survey_V2_Question_Kind_Enum,
            survey_question_options: [
              {
                id: '05b532b4-2e2a-49a3-8ade-215107c4d64f',
                position: 1,
                option: "I'm completely new to Hasura",
                template_config:
                  '{ "react_icons_fa_component_name": "FaHeart" }',
                additional_info_config: null,
              },
              {
                id: 'd6cd3f20-5add-4283-88c3-d112b7446369',
                position: 1,
                option:
                  "I've used Hasura before but not actively developing right now",
                template_config:
                  '{ "react_icons_fa_component_name": "FaBookmark" }',
                additional_info_config: null,
              },
              {
                id: '76c30481-a7cc-45b4-9bd0-7332a2fb8617',
                position: 1,
                option: "I'm already using Hasura (CE/Cloud) weekly/monthly",
                template_config:
                  '{ "react_icons_fa_component_name": "FaUser" }',
                additional_info_config: null,
              },
              {
                id: '319bb5c4-9aa6-42eb-afce-3862c4bef3a7',
                position: 1,
                option: "I'm actively developing with Hasura (CE/Cloud) daily",
                template_config:
                  '{ "react_icons_fa_component_name": "FaStar" }',
                additional_info_config: null,
              },
            ],
          },
        ],
        survey_responses: [
          {
            survey_response_answers: [
              {
                survey_question_id: '4595916a-1c5b-4d55-b7ca-11616131d1d3',
                survey_response_answer_options: [
                  {
                    answer: null,
                    additional_info: null,
                    option_id: '319bb5c4-9aa6-42eb-afce-3862c4bef3a7',
                  },
                ],
              },
            ],
          },
        ],
        template_config:
          '{ "survey_submit_text": "Submit", "survey_cancel_text": "Cancel" }',
      },
    ],
  },
};

export const mockMetadataUrl = getMetadataUrl(NEON_TEMPLATE_BASE_PATH);
export const mockMigrationUrl = getMigrationUrl(NEON_TEMPLATE_BASE_PATH);
export const mockSampleQueryUrl = getSampleQueriesUrl(NEON_TEMPLATE_BASE_PATH);
export const mockSchemaImageUrl = getSchemaImageUrl(NEON_TEMPLATE_BASE_PATH);

export const MOCK_INITIAL_METADATA = {
  version: 3,
  sources: [
    {
      name: 'default',
      kind: 'postgres',
      tables: [],
      configuration: {
        connection_info: {
          database_url:
            'postgres://postgres:postgrespassword@postgres:5432/postgres',
          isolation_level: 'read-committed',
          use_prepared_statements: false,
        },
      },
    },
    {
      name: 'fragrant-firefly',
      kind: 'postgres',
      tables: [
        {
          table: {
            name: 'article',
            schema: '_manytomany',
          },
          array_relationships: [
            {
              name: 'article_tags',
              using: {
                foreign_key_constraint_on: {
                  column: 'article_id',
                  table: {
                    name: 'article_tag',
                    schema: '_manytomany',
                  },
                },
              },
            },
          ],
        },
        {
          table: {
            name: 'article_tag',
            schema: '_manytomany',
          },
          object_relationships: [
            {
              name: 'article',
              using: {
                foreign_key_constraint_on: 'article_id',
              },
            },
            {
              name: 'tag',
              using: {
                foreign_key_constraint_on: 'tag_id',
              },
            },
          ],
        },
        {
          table: {
            name: 'tag',
            schema: '_manytomany',
          },
          array_relationships: [
            {
              name: 'article_tags',
              using: {
                foreign_key_constraint_on: {
                  column: 'tag_id',
                  table: {
                    name: 'article_tag',
                    schema: '_manytomany',
                  },
                },
              },
            },
          ],
        },
        {
          table: {
            name: 'sample_table',
            schema: 'public',
          },
        },
      ],
      configuration: {
        connection_info: {
          database_url: 'some_secret_db_url',
          isolation_level: 'read-committed',
          use_prepared_statements: false,
        },
      },
      customization: {
        naming_convention: 'hasura-default',
      },
    },
  ],
};

export const MOCK_METADATA_FILE_CONTENTS = {
  resource_version: 12,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'default',
        kind: 'postgres',
        tables: [
          {
            table: {
              name: 'customer',
              schema: 'public',
            },
            array_relationships: [
              {
                name: 'orders',
                using: {
                  foreign_key_constraint_on: {
                    column: 'customer_id',
                    table: {
                      name: 'order',
                      schema: 'public',
                    },
                  },
                },
              },
            ],
          },
          {
            table: {
              name: 'order',
              schema: 'public',
            },
            object_relationships: [
              {
                name: 'customer',
                using: {
                  foreign_key_constraint_on: 'customer_id',
                },
              },
            ],
          },
        ],
      },
    ],
  },
};

export const serverDownErrorMessage = {
  code: 'Service Unavailable',
  error: `The resource cannot be delivered`,
};

export const MOCK_MIGRATION_FILE_CONTENTS = `
CREATE TABLE "public"."customer" (
  "id" int4,
  "first_name" text,
  "last_name" text,
  "email" text,
  "phone" text,
  "username" text,
  "ip_address" text,
  PRIMARY KEY ("id")
);

CREATE TABLE "public"."order" (
  "id" int4,
  "transaction_id" text,
  "product" text,
  "purchase_price" text,
  "discount_price" text,
  "order_date" text,
  "customer_id" int4,
  PRIMARY KEY ("id")
);

ALTER TABLE "public"."order" ADD FOREIGN KEY ("customer_id") REFERENCES "public"."customer"("id");

INSERT INTO "public"."customer" ("id", "first_name", "last_name", "email", "phone", "username", "ip_address") VALUES
(1, 'Daisy', 'Syme', 'dsyme0@hp.com', '+20 (915) 874-5336', 'dsyme0', '42.14.173.181'),
(2, 'Berny', 'Linford', 'blinford1@odnoklassniki.ru', '+55 (659) 852-4292', 'blinford1', '84.112.166.217'),
(3, 'Krystal', 'Fretwell', 'kfretwell2@fda.gov', '+358 (577) 234-5107', 'kfretwell2', '16.230.140.234'),
(4, 'Donnell', 'Yve', 'dyve3@aboutads.info', '+81 (167) 244-5980', 'dyve3', '50.23.181.152'),
(5, 'Ola', 'Fretter', 'ofretter4@sitemeter.com', '+86 (690) 881-4182', 'ofretter4', '73.164.185.62'),
(6, 'Ximenes', 'Mote', 'xmote5@barnesandnoble.com', '+502 (536) 300-9224', 'xmote5', '175.219.84.213'),
(7, 'Ainslie', 'Davidzon', 'adavidzon6@cbslocal.com', '+254 (755) 803-4401', 'adavidzon6', '186.172.158.188'),
(8, 'Diego', 'Ellit', 'dellit7@walmart.com', '+62 (310) 893-0690', 'dellit7', '86.123.251.103'),
(9, 'Maximilien', 'Longbothom', 'mlongbothom8@yahoo.co.jp', '+33 (279) 317-1163', 'mlongbothom8', '5.68.95.19'),
(10, 'Garold', 'Pendock', 'gpendock9@foxnews.com', '+27 (948) 957-1398', 'gpendock9', '153.194.87.243');

INSERT INTO "public"."order" ("id", "transaction_id", "product", "purchase_price", "discount_price", "order_date", "customer_id") VALUES
(1, '1F3ZLKrcXyisvMa79GdQ3UCKcUpHprzkjX', 'Juice - Cranberry, 341 Ml', '$6.26 ', '$1.46 ', '10/19/2021', 7),
(2, '16WKVa7pV9xAUPC12ymbdFSA1TqLtfitSA', 'Pasta - Canelloni', '$4.74 ', '$1.82 ', '03/25/2022', 8),
(3, '19TbQiD2ijcDZCY9d3V8pxfj8U4punJxCo', 'Bread - Calabrese Baguette', '$9.83 ', '$0.45 ', '07/07/2022', 1),
(4, '1A2nhANZWqBcdCuz6BgTXNpiwCSrAMjbVQ', 'Ice Cream Bar - Rolo Cone', '$7.64 ', '$0.73 ', '12/26/2021', 2),
(5, '12VwQBESupdem6xHS1h4eqrvsffsyL3skU', 'Cookie - Oreo 100x2', '$9.03 ', '$1.83 ', '06/29/2022', 9),
(6, '1Co69PachjWpM2TfdnkrGuwaKpDDMRMBks', 'Bacardi Breezer - Tropical', '$2.43 ', '$0.10 ', '09/21/2022', 10),
(7, '16EXSRGT8iEgPiReq2rQfdAPDUm3wraM83', 'Lamb - Sausage Casings', '$8.06 ', '$1.26 ', '03/16/2022', 10),
(8, '1C64titz7BGGjt76nG9Vfzh3D4v8JjwyH5', 'Mini - Vol Au Vents', '$3.27 ', '$0.54 ', '01/15/2022', 5),
(9, '1H5Cujy9X9NgHfZf6rqeFXsczeUderCJUf', 'Table Cloth 54x54 Colour', '$9.26 ', '$1.08 ', '08/02/2022', 6),
(10, '14pLQTdLWX2rWZqqKqHosTdwwUX6iikgHD', 'Tomatoes - Hot House', '$2.61 ', '$1.27 ', '02/28/2022', 7),
(11, '1524M52setuYRjfdDF8xzqVg5TjL4bFrzo', 'Sauce - White, Mix', '$5.41 ', '$0.96 ', '01/06/2022', 2),
(12, '1GXisxNigWCmB1xKJjz5PY53wXVnhcysg', 'Sausage - Blood Pudding', '$3.22 ', '$1.99 ', '02/17/2022', 1),
(13, '1PSDKmzMg5BMXABNqRZxEVg59kaTsu169y', 'Oats Large Flake', '$9.17 ', '$1.29 ', '06/15/2022', 10),
(14, '1LjtJTJt3NPg9Sq8QHFUoYFNJZafLD751q', 'Cheese - Parmesan Cubes', '$1.89 ', '$1.42 ', '04/13/2022', 9),
(15, '1KfeywEXGGHvUPALHPDVQFnX2jXdu4J4fC', 'Split Peas - Yellow, Dry', '$7.32 ', '$0.93 ', '04/18/2022', 7),
(16, '158DLeiSkfDzR7JyEwRSfagmYkhzD1ZkYA', 'Flower - Potmums', '$1.95 ', '$0.18 ', '05/30/2022', 8),
(17, '14WcFhhLCF65XJ47LqUDyYkKtqp77tT29L', 'Lobster - Baby, Boiled', '$7.29 ', '$0.96 ', '11/17/2021', 9),
(18, '15B9rtow88Jzqf3t2LJepHd6hCEVd3M7Rd', 'Tuna - Loin', '$2.12 ', '$0.07 ', '02/21/2022', 2),
(19, '161qmk9yP269JvqMWFdWipbeBkGjAa5RLe', 'Soup - Campbells, Creamy', '$6.84 ', '$1.26 ', '08/28/2022', 4),
(20, '17xt1nVNKgtcfesgUWW2D1MB4fJMnqMcFk', 'Wine - Pinot Noir Pond Haddock', '$1.78 ', '$0.35 ', '03/11/2022', 3),
(21, '1JXmjRLRXKGfkmGt8E2ZTHTZrKVAa7sJ4J', 'Cake - Bande Of Fruit', '$8.68 ', '$1.24 ', '11/14/2021', 5),
(22, '16ALno1YEmGg4ZQcdt9NYwFwFeknMfHzrK', 'Pastry - Baked Cinnamon Stick', '$3.99 ', '$0.89 ', '02/15/2022', 1),
(23, '1EHcpDDkL3SCRNu3wJkH48QZVTtw3c1Hk8', 'Bread Crumbs - Panko', '$1.82 ', '$0.94 ', '09/24/2022', 3),
(24, '1Ew3i2S8ZrSumCiiDgVV7gvY3sN43R5mTn', 'Wine - White, Cooking', '$9.03 ', '$1.40 ', '09/25/2022', 5),
(25, '1PH7f4Y7oUH1c5xGuinmTPng3poHSiCCTm', 'Potatoes - Idaho 80 Count', '$7.64 ', '$0.10 ', '10/15/2021', 6),
(26, '1QE4zGaLEL49fRM5jspBgFQVsmP5AboAgR', 'Red Snapper - Fillet, Skin On', '$5.16 ', '$0.08 ', '04/16/2022', 5),
(27, '16ecF9SH1ySFAUn4dLxerAeyinccrDASdZ', 'Carbonated Water - White Grape', '$4.01 ', '$1.41 ', '11/02/2021', 4),
(28, '1Mg24XDM5zAXBxCyTU6Xe2SQCLZuRh9tMm', 'Energy Drink', '$1.99 ', '$0.01 ', '08/29/2022', 8),
(29, '19D5mnYpFGaWwPSN657ovWPzh1PHkjp4Wg', 'Table Cloth 54x54 White', '$7.23 ', '$0.76 ', '06/26/2022', 2),
(30, '1MYY8JqZk3BcwPsxPH2G8nEYqFNkMKFMSM', 'Muffin Hinge - 211n', '$4.25 ', '$1.25 ', '08/24/2022', 10),
(31, '1JvZyii5fzLn8mmrCWarjHxd8JE7rGBnXw', 'Soup - Campbells Beef Noodle', '$7.15 ', '$1.40 ', '01/08/2022', 6),
(32, '18HGUxeEAtYUkiWxN2QeTmR85ygoeamivb', 'Tea - English Breakfast', '$6.74 ', '$1.02 ', '11/20/2021', 1),
(33, '1ESV3zwy1V5Ff5z5T3TiqJuftbEg8Kc6nd', 'Lamb - Sausage Casings', '$3.31 ', '$0.84 ', '09/16/2022', 9),
(34, '17x7uCHL99C9FjyKZMvxoKM36jzKQTD8X2', 'Coconut - Shredded, Unsweet', '$7.71 ', '$0.15 ', '11/20/2021', 5),
(35, '1MeeiJYSkHnAVw8tNbnNwaw8sNn9DUjgr4', 'Chicken - Leg, Fresh', '$6.35 ', '$1.43 ', '04/10/2022', 3),
(36, '1Ka3cptwfsRyKYA33cNP45wYWA3po15E1A', 'Wine - Lamancha Do Crianza', '$7.37 ', '$1.69 ', '06/02/2022', 7),
(37, '13FcF6f4mmzXBPBum8gkETSj9jNiFkS5QX', 'Creme De Menthe Green', '$2.99 ', '$1.09 ', '01/22/2022', 4),
(38, '13PmWYQDUe2ewJF7MoHQLus17XQhyeYruf', 'Quail - Jumbo', '$5.21 ', '$0.53 ', '10/17/2021', 4),
(39, '1NbocYefavPmQ6wDRWVsnNZNwaqqVbqX5z', 'Chocolate - Pistoles, Lactee, Milk', '$6.80 ', '$0.95 ', '05/03/2022', 1),
(40, '1E9KeV7T6TNd1gkb1e7ePGYve7qTtbA6RC', 'Coconut - Creamed, Pure', '$6.19 ', '$0.41 ', '11/26/2021', 10),
(41, '1LRNuJ2HHkcaQ5pVpSU349MVM6yw1Wcs8T', 'Cattail Hearts', '$9.72 ', '$0.55 ', '09/10/2022', 7),
(42, '13B8jKKCvkk52or91b1tC8ipBF6QhaWi98', 'Soup - Beef Conomme, Dry', '$4.56 ', '$0.33 ', '05/23/2022', 2),
(43, '1B2q9pQVBYsv9iYXHLHVeijf6dwypti4Sp', 'Egg Patty Fried', '$6.31 ', '$0.98 ', '10/04/2021', 2),
(44, '1PnXRW7DtFbAQ922FY4rmFXjeXcYsnuv2x', 'Venison - Denver Leg Boneless', '$2.60 ', '$0.74 ', '09/21/2022', 9),
(45, '136p12cywvTMJUvmPDwmDf95fij2ZbRcPr', 'Island Oasis - Ice Cream Mix', '$2.95 ', '$1.53 ', '03/22/2022', 2),
(46, '1JYmm4ZmdPrCPBYCuWgAHYX8iG6WaWk8Fs', 'Ostrich - Fan Fillet', '$4.32 ', '$0.42 ', '04/10/2022', 9),
(47, '13o7QeoHwnwD91FcRXkVLg1Qg5v3Qye4BK', 'Plate - Foam, Bread And Butter', '$1.81 ', '$1.64 ', '06/19/2022', 4),
(48, '15bLFHs6uWNt7fWBtKM8gphnh7GrfJPugG', 'Foil Wrap', '$1.34 ', '$0.62 ', '08/29/2022', 3),
(49, '14PaKTHoj7y2gAc6YsE9jtxHZzjjFRwzb7', 'Bread - Assorted Rolls', '$3.87 ', '$1.49 ', '09/26/2022', 1);
`;
