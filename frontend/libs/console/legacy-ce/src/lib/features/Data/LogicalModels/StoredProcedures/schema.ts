import { z } from 'zod';

export const trackStoredProcedureValidationSchema = z.object({
  dataSourceName: z.string().min(1, 'Source is a required field!'),
  configuration: z.object({
    exposed_as: z.union([z.literal('query'), z.literal('mutation')]),
    custom_name: z.string().optional(),
  }),
  stored_procedure: z.any().refine(
    val => {
      return !!val;
    },
    {
      message: 'Stored Procedure is a required field!',
    }
  ),
  returns: z.string().min(1, 'Return type is required!'),
  arguments: z
    .array(
      z.object({
        name: z.string(),
        type: z.string(),
        nullable: z.boolean().optional(),
      })
    )
    .optional(),
});

export type AddStoredProcedureFormData = z.infer<
  typeof trackStoredProcedureValidationSchema
>;

export const defaultEmptyValues: AddStoredProcedureFormData = {
  dataSourceName: '',
  configuration: {
    exposed_as: 'query',
  },
  returns: '',
};
