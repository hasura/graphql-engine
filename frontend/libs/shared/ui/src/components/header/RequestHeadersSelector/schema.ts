import { clientHeaderSchema } from '@hasura/shared/types';
import { z } from 'zod';

export const requestHeadersSelectorSchema = z.array(clientHeaderSchema);

export type RequestHeadersSelectorSchema = z.infer<
  typeof requestHeadersSelectorSchema
>;
