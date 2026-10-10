export const isNotNull = <T>(arg: T): arg is Exclude<T, null> => {
  return arg !== null && arg !== undefined;
};

export const isNotDefined = (value: unknown) => {
  return value === null || value === undefined;
};

export const isNumberString = (str: unknown): str is string =>
  typeof str === 'string' && !Number.isNaN(Number(str));

export function isJsonString(str: string): boolean {
  try {
    JSON.parse(str);
  } catch (e) {
    return false;
  }

  return true;
}

export const isArrayString = (str: string) => {
  try {
    return Array.isArray(JSON.parse(str));
  } catch (e) {
    return false;
  }
};

export const isArray = (value: unknown): value is any[] => {
  return Array.isArray(value);
};

export const isObject = (value: unknown): value is Record<string, unknown> => {
  return typeof value === 'object' && value !== null;
};

export type TypedObjectValidator = (val: Record<string, unknown>) => boolean;

/*
  NOTE: this function is useful to assert on the type of an object and access it's properties
  See the tests for examples.
*/
export const isTypedObject = <T>(
  value: unknown,
  validator: TypedObjectValidator,
): value is T => isObject(value) && validator(value);

export const isString = (value: unknown): value is string => {
  return typeof value === 'string';
};

export const isNumber = (value: unknown): value is number => {
  return typeof value === 'number';
};

export const isFloat = (value: unknown): value is number => {
  return typeof value === 'number' && value % 1 !== 0;
};

export const isEmpty = (value: any) => {
  let empty = false;

  if (value === null || value === undefined) {
    empty = true;
  } else if (isArray(value)) {
    empty = value.length === 0;
  } else if (isObject(value)) {
    empty = !Object.keys(value).length;
  } else if (isString(value)) {
    empty = value === '';
  }

  return empty;
};

export const isEqual = (value1: any, value2: any) => {
  let equal = false;

  if (typeof value1 === typeof value2) {
    if (isArray(value1)) {
      equal = JSON.stringify(value1) === JSON.stringify(value2);
    } else if (isObject(value2)) {
      const value1Keys = Object.keys(value1);
      const value2Keys = Object.keys(value2);

      if (value1Keys.length === value2Keys.length) {
        equal = true;

        for (let i = 0; i < value1Keys.length; i++) {
          const key = value1Keys[i];
          if (!isEqual(value1[key], value2[key])) {
            equal = false;
            break;
          }
        }
      }
    } else {
      equal = value1 === value2;
    }
  }

  return equal;
};

/* ARRAY utils */
export const deleteArrayElementAtIndex = (array: unknown[], index: number) => {
  return array.splice(index, 1);
};

export const getLastArrayElement = <T = unknown>(array: T[]): T | undefined => {
  return array[array.length - 1];
};

export const arrayDiff = (arr1: unknown[], arr2: unknown[]) => {
  return arr1.filter((v) => !arr2.includes(v));
};

export function getAllJsonPaths(
  json: any,
  leafKeys: any[],
  prefix = '',
): (Record<string, any> | string)[] {
  const paths: (Record<string, any> | string)[] = [];

  const addPrefix = (subPath: string) => {
    return prefix + (prefix && subPath ? '.' : '') + subPath;
  };

  const handleSubJson = (subJson: any, newPrefix: string) => {
    const subPaths = getAllJsonPaths(subJson, leafKeys, newPrefix);

    subPaths.forEach((subPath: (typeof subPaths)[0]) => {
      paths.push(subPath);
    });

    if (!subPaths.length) {
      paths.push(newPrefix);
    }
  };

  if (isArray(json)) {
    json.forEach((subJson: any, i: number) => {
      handleSubJson(subJson, addPrefix(i.toString()));
    });
  } else if (isObject(json)) {
    Object.keys(json).forEach((key) => {
      if (leafKeys.includes(key)) {
        paths.push({ [addPrefix(key)]: json[key] });
      } else {
        handleSubJson(json[key], addPrefix(key));
      }
    });
  } else {
    paths.push(addPrefix(json));
  }

  return paths;
}
