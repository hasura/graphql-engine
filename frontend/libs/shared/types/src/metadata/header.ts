import z from 'zod';
import { HeaderFromEnv, HeaderFromValue } from './metadata';

export type HeaderConfig = HeaderFromValue | HeaderFromEnv;

export const clientHeaderSchema = z.object({
  name: z.string(),
  value: z.string(),
  type: z.enum(['env', 'value']),
});

export type ClientHeader = z.infer<typeof clientHeaderSchema>;

export const defaultHeader: ClientHeader = {
  name: '',
  value: '',
  type: 'value',
};
