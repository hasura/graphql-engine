declare module '@walmartlabs/json-to-simple-graphql-schema/lib' {
  export function jsonToSchema(options: {
    jsonInput: string;
    baseType: string;
  }): { value: string };
}

declare module 'microfiber';

declare module 'react-progress-bar-plus';

declare module 'graphiql-code-exporter/lib/snippets' {
  const snippets: any[];
  export default snippets;
}

// @hookform/resolvers@2's "./zod" export map has no "types" condition, so
// TS moduleResolution "bundler" can't resolve its declaration file even
// though it exists on disk (node_modules/@hookform/resolvers/zod/dist/index.d.ts).
// Re-declare zodResolver's shape here to match it.
declare module '@hookform/resolvers/zod' {
  import type {
    FieldValues,
    ResolverResult,
    UnpackNestedValue,
    ResolverOptions,
  } from 'react-hook-form';
  import type { z } from 'zod';

  export type Resolver = <T extends z.Schema<any, any>>(
    schema: T,
    schemaOptions?: Partial<z.ParseParams>,
    factoryOptions?: {
      mode?: 'async' | 'sync';
      rawValues?: boolean;
    },
  ) => <TFieldValues extends FieldValues, TContext>(
    values: UnpackNestedValue<TFieldValues>,
    context: TContext | undefined,
    options: ResolverOptions<TFieldValues>,
  ) => Promise<ResolverResult<TFieldValues>>;

  export const zodResolver: Resolver;
}
