import { inferSchema } from '@jsonhero/schema-infer';
import {
  LogicalModel,
  LogicalModelField,
} from '../../../hasura-metadata-types';
import { sanitizeGraphQLFieldNames } from '../../../../utils';

type ObjectSchema = {
  type: 'object';
  properties: {
    [key: string]: any;
  };
  required: string[];
};

type ArraySchema = {
  type: 'array';
  items: {
    properties: {
      [key: string]: any;
    };
    required: string[];
  };
};

const getLogicalModelsFromProperties = (
  collectionName: string,
  name: string,
  properties: ObjectSchema['properties'],
  requiredProperties: string[] = [],
  parentName = ''
): LogicalModel[] => {
  const logicalModels: LogicalModel[] = [];
  const fields: LogicalModelField[] = [];
  for (const [rawFieldName, fieldSchema] of Object.entries(properties)) {
    const fieldName = sanitizeGraphQLFieldNames(rawFieldName);
    const nullable = !requiredProperties.includes(fieldName);
    const logicalModelPath = parentName
      ? `${parentName}_${fieldName}`
      : fieldName;

    // Get scalars from MongoDB objectid and date objects
    const handleMongoDBFieldTypes = (
      properties: ObjectSchema['properties']
    ): { type: 'objectId' | 'date' | 'string' | 'none'; name?: string } => {
      if (!properties) {
        return { type: 'string' };
      }
      if (Object.prototype.hasOwnProperty.call(properties, '$oid')) {
        return { type: 'objectId' };
      }
      if (Object.prototype.hasOwnProperty.call(properties, '$date')) {
        return { type: 'date' };
      }
      return { type: 'none' };
    };

    if (fieldSchema.type === 'object') {
      // Seperate MongoDB objectid and date scalars from logical model objects
      const mongoDBFieldType = handleMongoDBFieldTypes(fieldSchema.properties);
      if (mongoDBFieldType.type !== 'none') {
        fields.push({
          name: fieldName,
          type: {
            scalar: mongoDBFieldType.type,
            nullable: false,
          },
        });
        continue;
      }
      // Make new logical model
      const newLogicalModels = getLogicalModelsFromProperties(
        collectionName,
        `${collectionName}_${logicalModelPath}`,
        fieldSchema.properties,
        fieldSchema.required,
        logicalModelPath
      );
      logicalModels.push(...newLogicalModels);
      fields.push({
        name: fieldName,
        type: {
          logical_model: `${collectionName}_${logicalModelPath}`,
          nullable,
        },
      });
    }

    if (fieldSchema.type === 'array') {
      // Throw error for mixed object / scalar array
      // Schema inferer returns anyOf if there are any type conflicts
      const hasNestedAnyOf = (function checkNestedAnyOf(obj: any): boolean {
        if (typeof obj !== 'object' || obj === null) return false;
        if ('anyOf' in obj) return true;
        return Object.values(obj).some(
          val => typeof val === 'object' && checkNestedAnyOf(val)
        );
      })(fieldSchema.items);
      if (hasNestedAnyOf) {
        throw new Error(
          `The array for field "${fieldName}" contains both multiple types (objects, string, int, etc.). Please check and ensure it only contains one for inference. \n Exact key with issue: "${logicalModelPath}"`
        );
      }

      // Array of objects
      if (fieldSchema.items.type === 'object') {
        // Check for special mongo scalars
        const mongoDBFieldType = handleMongoDBFieldTypes(
          fieldSchema.items.properties
        );
        if (mongoDBFieldType.type !== 'none') {
          fields.push({
            name: fieldName,
            type: {
              array: {
                scalar: mongoDBFieldType.type,
                nullable,
              },
            },
          });
        } else {
          // Make new logical model for array
          const newLogicalModels = getLogicalModelsFromProperties(
            collectionName,
            `${collectionName}_${logicalModelPath}`,
            fieldSchema.items.properties,
            fieldSchema.items?.required || [],
            logicalModelPath
          );

          logicalModels.push(...newLogicalModels);

          fields.push({
            name: fieldName,
            type: {
              array: {
                logical_model: `${collectionName}_${logicalModelPath}`,
                nullable,
              },
            },
          });
        }
        continue;
      }
      // Array of scalars
      if (fieldSchema.items.type !== 'object') {
        // Process scalar types in array
        // TODO: DRY this out with scalar processing below
        if (fieldSchema.items.type === 'string') {
          fields.push({
            name: fieldName,
            type: {
              array: {
                scalar: 'string',
                nullable,
              },
            },
          });
        }
        if (fieldSchema.items.type === 'integer') {
          fields.push({
            name: fieldName,
            type: {
              array: {
                scalar: 'int',
                nullable,
              },
            },
          });
        }
        if (fieldSchema.items.type === 'number') {
          fields.push({
            name: fieldName,
            type: {
              array: {
                scalar: 'double',
                nullable,
              },
            },
          });
        }
        if (fieldSchema.items.type === 'boolean') {
          fields.push({
            name: fieldName,
            type: {
              array: {
                scalar: 'bool',
                nullable,
              },
            },
          });
        }
      }
    }

    // Process scalars
    if (fieldSchema.type === 'string') {
      fields.push({
        name: fieldName,
        type: {
          scalar: 'string',
          nullable,
        },
      });
    }

    if (fieldSchema.type === 'integer') {
      fields.push({
        name: fieldName,
        type: {
          scalar: 'int',
          nullable,
        },
      });
    }

    if (fieldSchema.type === 'number') {
      fields.push({
        name: fieldName,
        type: {
          scalar: 'double',
          nullable,
        },
      });
    }

    if (fieldSchema.type === 'boolean') {
      fields.push({
        name: fieldName,
        type: {
          scalar: 'bool',
          nullable,
        },
      });
    }
  }
  return [
    {
      name,
      fields,
    },
    ...logicalModels,
  ];
};

const getLogicalModels = (
  collectionName: string,
  name: string,
  schema: ObjectSchema | ArraySchema
): LogicalModel[] => {
  const sanitizedModelName = sanitizeGraphQLFieldNames(name);
  if (schema.type === 'object') {
    return getLogicalModelsFromProperties(
      collectionName,
      sanitizedModelName,
      schema.properties,
      schema.required
    );
  }

  if (schema.type === 'array') {
    return getLogicalModelsFromProperties(
      collectionName,
      sanitizedModelName,
      schema.items.properties,
      schema.items.required
    );
  }

  return [];
};

export const inferLogicalModels = (
  collectionName: string,
  json: string
): LogicalModel[] => {
  const document = JSON.parse(json);
  const schema = inferSchema(document).toJSONSchema();

  const logicalModels: LogicalModel[] = getLogicalModels(
    collectionName,
    collectionName,
    schema as unknown as ObjectSchema | ArraySchema
  );

  return logicalModels;
};
