import { z } from 'zod';

export const addLogicalModelValidationSchema = z.object({
  dataSourceName: z.string(),
  name: z.string().min(1, 'Name is a required field'),
  fields: z.array(
    z.object({
      name: z.string().min(1, 'Field Name is a required field'),
      type: z.string().min(1, 'Type is a required field'),
      nullable: z.boolean(),
    })
  ),
});

export type AddLogicalModelFormData = z.infer<
  typeof addLogicalModelValidationSchema
>;

export const defaultEmptyValues: AddLogicalModelFormData = {
  name: '',
  dataSourceName: '',
  fields: [],
};
