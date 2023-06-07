import { z } from 'zod';
import { implement } from '../../../../utils/zodUtils';
import { NativeQueryForm } from './types';

const reqString = (name: string) => {
  return z
    .string({ required_error: `${name} is required` })
    .trim()
    .min(1, `${name} is required`);
};

export const schema = implement<NativeQueryForm>().with({
  root_field_name: reqString('Native Query Name'),
  comment: z.string().optional(),
  source: reqString('Database'),
  type: z.enum(['query', 'mutation']).default('query').optional(),
  arguments: z
    .object({
      name: reqString('Parameter Name'),
      type: reqString('Parameter Type'),
      description: z.string().optional(),
      nullable: z.boolean().optional(),
    })
    .array(),
  code: reqString('Sql Query'),
  returns: reqString('Query Return Type'),
});
