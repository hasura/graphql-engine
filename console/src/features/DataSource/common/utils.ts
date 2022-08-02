/* eslint-disable no-underscore-dangle */
import pickBy from 'lodash.pickby';
import get from 'lodash.get';
import { z, ZodSchema } from 'zod';
import { IntrospectedTable, Property, Ref } from '../types';
import { RunSQLResponse } from '../api';

export const isProperty = (
  value: Ref | Property | { oneOf: (Property | Ref)[] }
): value is Property => {
  return 'type' in value;
};

export const isRef = (
  value: Ref | Property | { oneOf: (Property | Ref)[] }
): value is Ref => {
  return Object.keys(value).includes('$ref');
};

export const isOneOf = (
  value: Ref | Property | { oneOf: (Property | Ref)[] }
): value is { oneOf: (Property | Ref)[] } => {
  return Object.keys(value).includes('oneOf');
};

export const getPropertyByRef = (
  property: Ref,
  otherSchemas: Record<string, Property>
) => {
  const ref = property.$ref;
  const _property = get(otherSchemas, ref.split('/').slice(2).join('.'));
  return _property;
};

export const getZodSchema = (
  property: Property,
  otherSchemas: Record<string, Property>
): ZodSchema => {
  if (property.type === 'string') {
    const t = z.string();

    if (!property.nullable)
      return t.min(
        1,
        property.description
          ? `${property.description} is Required`
          : 'Required!'
      );

    return t;
  }

  if (property.type === 'boolean') {
    return z.preprocess(x => {
      if (!x) return false;
      return true;
    }, z.boolean());
  }

  if (property.type === 'number') {
    if (!property.nullable)
      return z
        .string()
        .min(1, 'Required')
        .transform(x => parseInt(x, 10) || '');
    return z.string().transform(x => parseInt(x, 10) || '');
  }

  if (property.type === 'object')
    return z
      .object({
        ...Object.entries(property.properties).reduce(
          (acc, [key, _property]) => {
            if (isRef(_property)) {
              const _refProperty = getPropertyByRef(_property, otherSchemas);
              return {
                ...acc,
                [key]: getZodSchema(_refProperty, otherSchemas),
              };
            }
            // console.log(_property, "here!1");

            if (isOneOf(_property)) {
              // console.log(_property, "here!");
              let _unions: ZodSchema = z.void();

              _property.oneOf.forEach((_oneOfProperty, i) => {
                // eslint-disable-next-line @typescript-eslint/no-shadow
                let temp: Property;
                if (isRef(_oneOfProperty))
                  temp = getPropertyByRef(_oneOfProperty, otherSchemas);
                else temp = _oneOfProperty;

                if (i === 0) {
                  _unions = getZodSchema(temp, otherSchemas);
                } else {
                  _unions = _unions.or(getZodSchema(temp, otherSchemas));
                }
              });

              return { ...acc, [key]: _unions };
            }

            return {
              ...acc,
              [key]: getZodSchema(_property, otherSchemas),
            };
          },
          {}
        ),
      })
      .transform((value: any) => pickBy(value, (d: any) => d !== ''));

  return z.void();
};

export const adaptIntrospectedTables = (
  runSqlResponse: RunSQLResponse
): IntrospectedTable[] => {
  /* 
    The `slice(1)` on the result is done because the first item of the result is always the columns names from the SQL output.
    It is not required for the final result and should be avoided 
  */
  const adaptedResponse = runSqlResponse?.result
    ?.slice(1)
    .map((row: string[]) => ({
      name: `${row[1]}.${row[0]}`,
      table: {
        name: row[0],
        schema: row[1],
      },
      type: row[2],
    }));

  return adaptedResponse ?? [];
};
