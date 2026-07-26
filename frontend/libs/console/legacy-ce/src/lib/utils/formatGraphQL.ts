import { print, parse } from 'graphql';

/**
 * Format and validate from a raw GraphQL string.
 * @param schemaSdl A raw GraphQL string.
 * @returns A formatted GraphQL string.
 */
export function formatGraphQL(schemaSdl: string): string {
  return print(parse(schemaSdl));
}
