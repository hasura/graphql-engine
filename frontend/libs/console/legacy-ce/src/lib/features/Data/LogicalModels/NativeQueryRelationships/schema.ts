import { z } from 'zod';

export const nativeQueryRelationshipValidationSchema = z.object({
  name: z.string().min(1, 'Relationship name required!'),
  toNativeQuery: z.string().min(1, 'Target Native Query is required!'),
  type: z.union([z.literal('object'), z.literal('array')]),
  columnMapping: z.record(z.string()),
});

export type NativeQueryRelationshipFormSchema = z.infer<
  typeof nativeQueryRelationshipValidationSchema
>;
