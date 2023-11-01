import { default as handleAsyncError } from 'await-to-js';
import { useQuery } from 'react-query';
import { z } from 'zod';
import { DataSource, Feature } from '../../../DataSource';
import { useHttpClient } from '../../../Network';
import { transformSchemaToZodObject } from '../../../OpenApi3Form/utils';
import { graphQLCustomizationSchema } from '../GraphQLCustomization/schema';
import { OpenApiSchema } from '@hasura/dc-api-types';
import { reqString } from '../../../../utils/zodUtils';

type GDCConfigSchemas = {
  configSchema: OpenApiSchema;
  otherSchemas: Record<string, OpenApiSchema>;
};

const createValidationSchema = (configSchemas: GDCConfigSchemas) =>
  z.object({
    name: z.string().min(1, 'Name is a required field!'),
    configuration: transformSchemaToZodObject(
      configSchemas.configSchema,
      configSchemas.otherSchemas
    ),
    customization: graphQLCustomizationSchema.optional(),
    timeout: z.coerce
      .number()
      .gte(0, { message: 'Timeout must be a postive number' })
      .optional(),
    template: z.string().optional(),

    // template variables is not marked as optional b/c it makes some pretty annoying TS issues with react-hook-form
    // the field is initialized with a default value of `[]`
    // with clean up empty fields, including arrays before submission, so it won't be sent to the server if the array is empty
    template_variables: z
      .object({
        name: reqString('variable name'),
        type: reqString('type'),
        filepath: reqString('filepath'),
      })
      .array(),
  });

export type GDCFormSchema = z.infer<ReturnType<typeof createValidationSchema>>;
export type TemplateVariableMap = Record<
  string,
  { type: string; filepath: string }
>;
// this takes care of adapting the template variables from an array to a map
export const templateVariableArrayToMap = (
  variableArray: GDCFormSchema['template_variables']
): TemplateVariableMap => {
  try {
    return variableArray.reduce<TemplateVariableMap>((map, obj) => {
      const { name, ...rest } = obj;
      map[name] = { ...rest };
      return map;
    }, {});
  } catch (e) {
    console.warn('Error converting template variable array to map:', e);
    return {};
  }
};

export const useFormValidationSchema = (driver: string) => {
  const httpClient = useHttpClient();
  return useQuery({
    queryKey: ['form-schema', driver],
    queryFn: async () => {
      const [err, configSchemas] = await handleAsyncError(
        DataSource(httpClient).connectDB.getConfigSchema(driver)
      );

      if (err) {
        throw err;
      }

      if (!configSchemas || configSchemas === Feature.NotImplemented)
        throw Error('Could not retrieve config schema info for driver');

      const validationSchema = createValidationSchema(configSchemas);

      return { validationSchema, configSchemas };
    },
    refetchOnWindowFocus: false,
  });
};
