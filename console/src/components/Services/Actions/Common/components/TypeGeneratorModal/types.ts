import z from 'zod';
import { schema } from './TypeGeneratorForm';

export type SchemaType = z.infer<typeof schema>;
