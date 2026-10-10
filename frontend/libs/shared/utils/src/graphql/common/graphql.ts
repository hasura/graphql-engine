import { print, parse } from 'graphql';

/**
 * Format and validate from a raw GraphQL string.
 * @param schemaSdl A raw GraphQL string.
 * @returns A formatted GraphQL string.
 */
export function formatGraphQL(schemaSdl: string): string {
  return print(parse(schemaSdl));
}

export const gqlPattern = /^[_A-Za-z][_0-9A-Za-z]*$/;

export const gqlTableErrorNotif = [
  'Error creating table!',
  'Table name cannot contain special characters',
  {
    custom:
      'Table name cannot contain special characters. It can have letters, numbers and _ (cannot start with numbers)',
  },
  'Error renaming table!',
] as const;

export const gqlColumnErrorNotif = [
  'Error adding column!',
  'Column name cannot contain special characters',
  {
    custom:
      'Column name cannot contain special characters. It can have letters, numbers and _ (cannot start with numbers)',
  },
  'Error renaming column!',
] as const;

export const gqlViewErrorNotif = [
  'Error creating view!',
  'View name cannot contain special characters',
  {
    custom:
      'View name cannot contain special characters. It can have letters, numbers and _ (cannot start with numbers)',
  },
  'Error renaming view!',
] as const;

export const gqlRelErrorNotif = [
  'Error adding relationship!',
  'Relationship name cannot contain special characters',
  {
    custom:
      'Relationship name cannot contain special characters. It can have letters, numbers and _ (cannot start with numbers)',
  },
  'Error renaming relationship!',
] as const;

export const gqlSchemaErrorNotif = [
  'Error creating schema!',
  'Schema name cannot contain special characters',
  {
    custom:
      'Schema name cannot contain special characters. It can have letters, numbers and _ (cannot start with numbers)',
  },
] as const;

const sanitizeGraphQLPattern = /[^\w]/g;

export const sanitizeGraphQLFieldNames = (value: string): string => {
  return value.replace(/ /g, '_').replace(sanitizeGraphQLPattern, '');
};
