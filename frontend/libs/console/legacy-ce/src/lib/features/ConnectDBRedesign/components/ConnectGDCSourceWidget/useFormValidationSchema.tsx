import { useQuery } from '@tanstack/react-query';
import { z } from 'zod';
import {
  getDatabaseMethods,
  NotImplementedError,
} from '@hasura/metadata/data-source';
import { graphQLCustomizationSchema } from '../GraphQLCustomization/schema';
import { OpenApiSchema } from '@hasura/dc-api-types';
import { reqString } from '@hasura/shared/utils';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { transformSchemaToZodObject } from '@hasura/shared/ui';

type GDCConfigSchemas = {
  configSchema: OpenApiSchema;
  otherSchemas: Record<string, OpenApiSchema>;
};

const createValidationSchema = (configSchemas: GDCConfigSchemas) =>
  z.object({
    name: z.string().min(1, 'Name is a required field!'),
    configuration: transformSchemaToZodObject(
      configSchemas.configSchema,
      configSchemas.otherSchemas,
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
  variableArray: GDCFormSchema['template_variables'],
): TemplateVariableMap => {
  try {
    return variableArray.reduce<TemplateVariableMap>((map, obj) => {
      if (!obj.name || (!obj.type && !obj.filepath)) {
        return map;
      }

      map[obj.name] = {
        type: obj.type ?? '',
        filepath: obj.filepath ?? '',
      };

      return map;
    }, {});
  } catch (e) {
    console.warn('Error converting template variable array to map:', e);
    return {};
  }
};

export const useFormValidationSchema = (driver: string) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  return useQuery({
    queryKey: ['form-schema', driver],
    queryFn: async () => {
      const dbMethods = getDatabaseMethods(driver);
      if (!dbMethods.introspection?.getDatabaseConfiguration) {
        throw new NotImplementedError(
          'Could not retrieve config schema info for driver',
        );
      }

      const configSchemas =
        await dbMethods.introspection.getDatabaseConfiguration({
          driver,
          endpoints,
          fetchJson,
        });

      const validationSchema = createValidationSchema(configSchemas);

      return { validationSchema, configSchemas };
    },
    refetchOnWindowFocus: false,
  });
};
