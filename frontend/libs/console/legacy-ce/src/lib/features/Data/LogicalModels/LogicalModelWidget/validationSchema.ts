import { z } from 'zod';

export const addLogicalModelValidationSchema = z.object({
  dataSourceName: z.string().min(1, 'Source is required'),
  name: z.string().min(1, 'Name is a required field'),
  fields: z
    .object({
      name: z.string().min(1, 'Field Name is a required field'),
      type: z.string().min(1, 'Type is a required field'),
      typeClass: z.enum(['scalar', 'logical_model']),
      array: z.boolean(),
      nullable: z.boolean({ required_error: 'Nullable is a required field' }),
    })
    .array(),
});

export type AddLogicalModelFormData = z.infer<
  typeof addLogicalModelValidationSchema
>;

export type AddLogicalModelField = AddLogicalModelFormData['fields'][number];

export const defaultEmptyValues: AddLogicalModelFormData = {
  name: '',
  dataSourceName: '',
  fields: [],
};
