// Ported from graphql-scalars (MIT): https://github.com/graphql-hive/graphql-scalars
// Only the two scalars used by OpenAPI-to-GraphQL are kept, so the library
// doesn't pull in graphql-scalars and its separate CJS `graphql` instance.

import {
  GraphQLError,
  GraphQLScalarType,
  Kind,
  print,
  type ObjectValueNode,
  type ValueNode,
} from 'graphql';

// --------------------------------------------------
// JSON
// --------------------------------------------------

function parseObject(
  ast: ObjectValueNode,
  variables: Record<string, unknown> | null | undefined,
): Record<string, unknown> {
  const value = Object.create(null);
  ast.fields.forEach((field) => {
    value[field.name.value] = parseJSONLiteral(field.value, variables);
  });
  return value;
}

function parseJSONLiteral(
  ast: ValueNode,
  variables: Record<string, unknown> | null | undefined,
): unknown {
  switch (ast.kind) {
    case Kind.STRING:
    case Kind.BOOLEAN:
      return ast.value;
    case Kind.INT:
    case Kind.FLOAT:
      return parseFloat(ast.value);
    case Kind.OBJECT:
      return parseObject(ast, variables);
    case Kind.LIST:
      return ast.values.map((n) => parseJSONLiteral(n, variables));
    case Kind.NULL:
      return null;
    case Kind.VARIABLE:
      return variables ? variables[ast.name.value] : undefined;
    default:
      return undefined;
  }
}

const jsonSpecifiedByURL =
  'http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf';

export const GraphQLJSON = new GraphQLScalarType({
  name: 'JSON',
  description:
    'The `JSON` scalar type represents JSON values as specified by [ECMA-404](http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf).',
  serialize: (value) => value,
  parseValue: (value) => value,
  parseLiteral: parseJSONLiteral,
  specifiedByURL: jsonSpecifiedByURL,
  extensions: {
    codegenScalarType: 'any',
  },
});

// --------------------------------------------------
// BigInt
// --------------------------------------------------

function isObjectLike(value: unknown): value is Record<string, any> {
  return typeof value === 'object' && value !== null;
}

// Unwrap objects such as `Number`/`BigInt` wrappers or values with `toJSON()`
function serializeObject(outputValue: unknown): unknown {
  if (isObjectLike(outputValue)) {
    if (typeof outputValue.valueOf === 'function') {
      const valueOfResult = outputValue.valueOf();
      if (!isObjectLike(valueOfResult)) {
        return valueOfResult;
      }
    }
    if (typeof outputValue.toJSON === 'function') {
      return outputValue.toJSON();
    }
  }
  return outputValue;
}

let warnedAboutBigIntJSON = false;

function serializeSafeBigInt(value: bigint): number | bigint | string {
  if (value <= Number.MAX_SAFE_INTEGER && value >= Number.MIN_SAFE_INTEGER) {
    return Number(value);
  }
  if ('toJSON' in BigInt.prototype) {
    return value;
  }
  if (!warnedAboutBigIntJSON) {
    warnedAboutBigIntJSON = true;
    console.warn(
      'By default, BigInts are not serialized to JSON as numbers but instead as strings which may lead an unintegrity in your data. ' +
        'To fix this, you can use "json-bigint-patch" to enable correct serialization for BigInts.',
    );
  }
  return value.toString();
}

export const GraphQLBigInt = new GraphQLScalarType({
  name: 'BigInt',
  description:
    'The `BigInt` scalar type represents non-fractional signed whole numeric values.',
  serialize(outputValue) {
    const coercedValue = serializeObject(outputValue);
    let num: unknown = coercedValue;

    if (isObjectLike(coercedValue) && 'toString' in coercedValue) {
      num = BigInt(coercedValue.toString());
      if ((num as bigint).toString() !== coercedValue.toString()) {
        throw new GraphQLError(
          `BigInt cannot represent non-integer value: ${coercedValue}`,
        );
      }
    }
    if (typeof coercedValue === 'boolean') {
      num = BigInt(coercedValue);
    }
    if (typeof coercedValue === 'string' && coercedValue !== '') {
      num = BigInt(coercedValue);
      if ((num as bigint).toString() !== coercedValue) {
        throw new GraphQLError(
          `BigInt cannot represent non-integer value: ${coercedValue}`,
        );
      }
    }
    if (typeof coercedValue === 'number') {
      if (!Number.isInteger(coercedValue)) {
        throw new GraphQLError(
          `BigInt cannot represent non-integer value: ${coercedValue}`,
        );
      }
      num = BigInt(coercedValue);
    }
    if (typeof num !== 'bigint') {
      throw new GraphQLError(
        `BigInt cannot represent non-integer value: ${coercedValue}`,
      );
    }
    return serializeSafeBigInt(num);
  },
  parseValue(inputValue) {
    const bigint = BigInt(String(inputValue));
    if (String(inputValue) !== bigint.toString()) {
      throw new GraphQLError(`BigInt cannot represent value: ${inputValue}`);
    }
    return bigint;
  },
  parseLiteral(valueNode) {
    if (!('value' in valueNode)) {
      throw new GraphQLError(
        `BigInt cannot represent non-integer value: ${print(valueNode)}`,
        { nodes: valueNode },
      );
    }
    const strOrBooleanValue = valueNode.value;
    const bigint = BigInt(strOrBooleanValue);
    if (strOrBooleanValue.toString() !== bigint.toString()) {
      throw new GraphQLError(
        `BigInt cannot represent value: ${strOrBooleanValue}`,
      );
    }
    return bigint;
  },
  extensions: {
    codegenScalarType: 'bigint',
    jsonSchema: {
      type: 'integer',
      format: 'int64',
    },
  },
});
