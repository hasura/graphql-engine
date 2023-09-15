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

  type ItemSchemaTypes = {
    anyOf?: Array<{ type: string }>;
    type?: string;
  };
  const isMixedArray = (itemsSchema: ItemSchemaTypes): boolean => {
    if (itemsSchema.anyOf) {
      const types = itemsSchema.anyOf.map(subSchema => subSchema.type);
      return (
        types.includes('object') && !types.every(type => type === 'object')
      );
    }
    return false;
  };

  for (const [rawFieldName, fieldSchema] of Object.entries(properties)) {
    const fieldName = sanitizeGraphQLFieldNames(rawFieldName);

    if (fieldName === '_id') {
      fields.push({
        name: fieldName,
        type: {
          scalar: 'objectId',
          nullable: false,
        },
      });
      continue;
    }

    const nullable = !requiredProperties.includes(fieldName);
    const logicalModelPath = parentName
      ? `${parentName}_${fieldName}`
      : fieldName;

    if (fieldSchema.type === 'object') {
      if (fieldSchema.properties) {
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
      } else {
        // Empty object just being casted to `string`
        fields.push({
          name: fieldName,
          type: {
            scalar: 'string',
            nullable,
          },
        });
      }
    }

    if (fieldSchema.type === 'array') {
      if (isMixedArray(fieldSchema.items)) {
        throw new Error(
          `The array for field "${fieldName}" contains both objects and scalars (string, int, etc.). Please check and ensure it only contains one for inference. \n Exact key with issue: "${logicalModelPath}"`
        );
      }
      if (fieldSchema.items.type === 'object') {
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
      } else {
        // scalar array
        fields.push({
          name: fieldName,
          type: {
            array: {
              scalar: fieldSchema.items.type,
              nullable,
            },
          },
        });
      }
    }

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
