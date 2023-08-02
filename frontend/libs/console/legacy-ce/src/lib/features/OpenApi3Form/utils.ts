/* eslint-disable no-underscore-dangle */
import get from 'lodash/get';
import { OpenApiSchema, OpenApiReference } from '@hasura/dc-api-types';
import { z, ZodSchema } from 'zod';
import pickBy from 'lodash/pickBy';

export function isReferenceObject(obj: any): obj is OpenApiReference {
  return Object.prototype.hasOwnProperty.call(obj, '$ref');
}

export const getReferenceObject = (
  ref: string,
  references: Record<string, OpenApiSchema>
): OpenApiSchema => {
  return get(references, ref.split('/').slice(2).join('.'));
};

export const getStringZodSchema = (schema: OpenApiSchema): ZodSchema => {
  /**
   * Only if the schema explicitly says that it can be empty, the zod schema can be optional
   */
  if (schema.nullable === true) return z.string().optional();

  return z.string().min(1, `${schema.title ?? 'value'} cannot be empty`);
};

export const getEnumZodSchema = (schema: OpenApiSchema): ZodSchema => {
  const enumOptions = schema.enum ?? [];

  const literals = enumOptions.map(enumValue => z.literal(enumValue));

  return z
    .union([z.literal('null'), ...literals] as any)
    .transform((value, ctx) => {
      if (value === 'null') return null;
      if (value) return value;

      ctx.addIssue({
        code: z.ZodIssueCode.custom,
        message: 'value not part of the enum list',
      });

      return z.never;
    });
};

export const getBooleanZodSchema = (schema: OpenApiSchema): ZodSchema => {
  if (schema.nullable === true) return z.boolean().optional();

  return z.union([z.boolean(), z.undefined()]).transform(value => {
    return !!value;
  });
};

export const getNumberZodSchema = (schema: OpenApiSchema): ZodSchema => {
  if (schema.nullable === true) return z.number().optional();
  return z.number();
};

export const getArrayZodSchema = (
  schema: OpenApiSchema,
  references: Record<string, OpenApiSchema>
): ZodSchema => {
  const items = schema.items;
  if (!items)
    throw Error(
      "Unable to find a 'items' in the schema object of type 'array'"
    );

  const itemSchema = isReferenceObject(items)
    ? getReferenceObject(items.$ref, references)
    : items;

  /**
   * Enum
   */
  if (itemSchema.type === 'string' && itemSchema.enum) {
    /**
     * No nullable case in enum, as mentioned in doc -
     * Note that null must be explicitly included in the list of enum values. Using nullable: true alone is not enough here.
     */
    return getEnumZodSchema(schema);
  }

  /**
   * String Array
   */
  if (itemSchema.type === 'string') {
    if (schema.nullable === true) return z.array(z.string()).optional();
    return z.array(z.string()).nonempty({
      message: 'List cannot be empty!',
    });
  }

  /**
   * Number Array
   */
  if (itemSchema.type === 'number' || itemSchema.type === 'integer') {
    if (schema.nullable === true)
      return z
        .string()
        .transform(value =>
          value.split(',').map(val => parseInt(val, 10) || '')
        )
        .optional();
    return z
      .string()
      .transform(value => value.split(',').map(val => parseInt(val, 10) || ''));
  }

  if (itemSchema.type !== 'object')
    throw Error(`No compatible zod schema found for ${itemSchema.type}`);

  /**
   * Object Array
   */
  if (schema.nullable === true)
    return z
      .array(transformSchemaToZodObject(itemSchema, references))
      .optional();

  return z
    .array(transformSchemaToZodObject(itemSchema, references))
    .nonempty(`${schema.title ?? 'value'} must contain at least one entry`);
};

export const transformSchemaToZodObject = (
  schema: OpenApiSchema,
  references: Record<string, OpenApiSchema>
): ZodSchema => {
  let zodSchema: ZodSchema = z.any();

  const type = schema.type;

  if (type === 'string' && schema.enum) return getEnumZodSchema(schema);

  if (type === 'string') return getStringZodSchema(schema);

  if (type === 'number' || type === 'integer')
    return getNumberZodSchema(schema);

  if (type === 'boolean') return getBooleanZodSchema(schema);

  if (type === 'array') {
    const items = schema.items;
    if (!items)
      throw Error(
        "Unable to find a 'items' in the schema object of type 'array'"
      );

    const itemSchema = isReferenceObject(items)
      ? getReferenceObject(items.$ref, references)
      : items;

    if (itemSchema.type === 'string') {
      if (schema.nullable === true) return zodSchema;
      return z.array(z.string()).nonempty({
        message: 'List cannot be empty!',
      });
    }

    if (itemSchema.type === 'number' || itemSchema.type === 'integer')
      zodSchema = z
        .string()
        .transform(value =>
          value.split(',').map(val => parseInt(val, 10) || '')
        );

    if (itemSchema.type === 'object') {
      if (schema.nullable === true)
        return z
          .array(transformSchemaToZodObject(itemSchema, references))
          .optional();

      return z
        .array(transformSchemaToZodObject(itemSchema, references))
        .nonempty(`${schema.title ?? 'value'} must contain at least one entry`);
    }
  }

  if (type === 'object') {
    const properties = schema.properties;
    /**
     * Free form objects
     */
    if (!properties) {
      zodSchema = z.any().transform((value, ctx) => {
        try {
          if (typeof value === 'string') return JSON.parse(value);
          return value;
        } catch {
          ctx.addIssue({
            code: z.ZodIssueCode.custom,
            message: 'Not a valid JSON',
          });
        }
      });
    } else {
      /**
       * Object with properties
       */
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      const schemas = Object.entries(properties)
        .map<[string, ZodSchema]>(([name, property]) => {
          const propertySchema: OpenApiSchema = isReferenceObject(property)
            ? getReferenceObject(property.$ref, references)
            : property;

          return [name, transformSchemaToZodObject(propertySchema, references)];
        })
        .reduce<Record<string, ZodSchema>>((zodObject, [name, _zodSchema]) => {
          zodObject[name] = _zodSchema;
          return zodObject;
        }, {});
      zodSchema = z
        .object(schemas)
        .transform(value => pickBy(value, d => d !== ''));
    }
  }

  if (schema.oneOf) {
    const schemas = schema.oneOf.map(oneOfProperty => {
      const oneOfPropertySchema = isReferenceObject(oneOfProperty)
        ? getReferenceObject(oneOfProperty.$ref, references)
        : oneOfProperty;

      const _zodSchema = transformSchemaToZodObject(
        oneOfPropertySchema,
        references
      );

      return _zodSchema;
    });

    // https://github.com/colinhacks/zod/issues/831
    const union = z.union(schemas as any);

    return union;
  }

  if (schema.nullable === true) zodSchema = z.optional(zodSchema);

  return zodSchema;
};

export const getInputAttributes = (name: string, schema: OpenApiSchema) => {
  return {
    label: schema.title ?? name.split('.')[name.split('.').length - 1],
    tooltip: schema.description,
  };
};
