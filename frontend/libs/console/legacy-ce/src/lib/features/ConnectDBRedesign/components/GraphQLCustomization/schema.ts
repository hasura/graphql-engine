import { z } from 'zod';

export const graphQLCustomizationSchema = z.object({
  rootFields: z
    .object({
      namespace: z.string().optional(),
      prefix: z.string().optional(),
      suffix: z.string().optional(),
    })
    .optional(),
  typeNames: z
    .object({
      prefix: z.string().optional(),
      suffix: z.string().optional(),
    })
    .optional(),
  namingConvention: z.string().optional(),
});

export type GraphQLCustomizationSchema = z.infer<
  typeof graphQLCustomizationSchema
>;
