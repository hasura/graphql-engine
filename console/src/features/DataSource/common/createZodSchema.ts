import pickBy from 'lodash.pickby';

import { z, ZodSchema } from 'zod';
import type { OneOf, Property, Ref } from '../types';
import { getProperty, isOneOf, isRef } from './utils';

function createZodObject(
  properties: [string, Property | Ref | OneOf][],
  otherSchemas: Record<string, Property>
) {
  const zodObject = properties.reduce<Record<string, ZodSchema>>(
    (acc, [key, property]) => {
      if (isRef(property)) {
        const refProperty = getProperty(property, otherSchemas);
        acc[key] = createZodSchema(refProperty, otherSchemas);
        return acc;
      }

      if (isOneOf(property)) {
        const unions = property.oneOf.reduce<ZodSchema>(
          (unionsAcc, oneOfProperty, i) => {
            const p = getProperty(oneOfProperty, otherSchemas);

            if (i === 0) {
              return createZodSchema(p, otherSchemas);
            }

            return unionsAcc.or(createZodSchema(p, otherSchemas));
          },
          z.void()
        );
        acc[key] = unions;
        return acc;
      }

      acc[key] = createZodSchema(property, otherSchemas);
      return acc;
    },
    {}
  );

  return z.object(zodObject);
}

export function createZodSchema(
  property: Property,
  otherSchemas: Record<string, Property>
): ZodSchema {
  switch (property.type) {
    case 'string':
      const t = z.string();

      if (!property.nullable)
        return t.min(
          1,
          property.description
            ? `${property.description} is Required`
            : 'Required!'
        );

      return t;

    case 'boolean':
      return z.preprocess(x => !!x, z.boolean());

    case 'number':
      if (!property.nullable)
        return z
          .string()
          .min(1, 'Required')
          .transform(x => parseInt(x, 10) || '');
      return z.string().transform(x => parseInt(x, 10) || '');

    case 'object':
      const propertiesArray = Object.entries(property.properties);
      const zodObject = createZodObject(propertiesArray, otherSchemas);

      if (property.nullable) {
        // objects are required by default
        // therefore if object is nullable we need to wrap it in optional
        return z
          .optional(zodObject)
          .transform(value => pickBy(value, d => d !== ''));
      }

      return zodObject.transform(value => pickBy(value, d => d !== ''));

    default:
      return z.void();
  }
}
