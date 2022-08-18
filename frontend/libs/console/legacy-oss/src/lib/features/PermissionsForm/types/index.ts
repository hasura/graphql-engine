import * as z from 'zod';
import { schema } from '../utils/formSchema';

export type QueryType = 'insert' | 'select' | 'update' | 'delete';
export type AccessType =
  | 'fullAccess'
  | 'noAccess'
  | 'partialAccess'
  | 'partialAccessWarning';

export type FormOutput = z.infer<typeof schema>;
