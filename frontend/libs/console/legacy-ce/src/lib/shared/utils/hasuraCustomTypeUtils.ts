import type {
  CustomTypes,
  EnumType,
  InputObjectType,
  ObjectType,
  ScalarType,
} from '@hasura/shared/types';

export const inbuiltTypes = {
  Int: true,
  Boolean: true,
  String: true,
  Float: true,
  ID: true,
};

export const filterNameLessTypeLess = (arr) => {
  return arr.filter((item) => !!item.name && !!item.type);
};

export const filterNameless = (arr) => {
  return arr.filter((item) => !!item.name);
};

export const filterValueLess = (arr) => {
  return arr.filter((item) => !!item.value);
};

export const mergeFlatCustomTypes = (
  newTypes: FlattenCustomType[],
  existingTypes: FlattenCustomType[],
) => {
  const mergedTypes = [...existingTypes];
  const overlappingTypeNames: string[] = [];
  const existingTypeIndexMap = {};

  mergedTypes.forEach((et, i) => {
    existingTypeIndexMap[et.definition.name] = i;
  });

  newTypes.forEach((nt) => {
    if (existingTypeIndexMap[nt.definition.name] !== undefined) {
      mergedTypes[existingTypeIndexMap[nt.definition.name]] = nt;
      overlappingTypeNames.push(nt.definition.name);
    } else {
      mergedTypes.push(nt);
    }
  });

  return {
    types: mergedTypes,
    overlappingTypeNames,
  };
};

export type FlattenCustomType =
  | {
      kind: 'scalars';
      definition: ScalarType;
    }
  | {
      kind: 'input_objects';
      definition: InputObjectType;
    }
  | {
      kind: 'objects';
      definition: ObjectType;
    }
  | {
      kind: 'enums';
      definition: EnumType;
    };

export const flattenCustomTypes = (
  customTypesServer: CustomTypes | undefined,
): FlattenCustomType[] => {
  const customTypesClient: FlattenCustomType[] = [];
  if (!customTypesServer) {
    return customTypesClient;
  }

  Object.keys(customTypesServer).forEach((t) => {
    const tk = t as keyof CustomTypes;
    const definitions = customTypesServer[tk];
    if (definitions) {
      definitions.forEach((definition) => {
        customTypesClient.push({
          definition,
          kind: tk,
        } as FlattenCustomType);
      });
    }
  });

  return customTypesClient;
};

export const reconstructCustomTypes = (
  items: FlattenCustomType[],
): CustomTypes => {
  const customTypes: CustomTypes = {
    scalars: [],
    input_objects: [],
    objects: [],
    enums: [],
  };

  items.forEach((item) => {
    if (customTypes[item.kind]) {
      customTypes[item.kind]?.push(item.definition as any);
    }
  });

  return customTypes;
};

export const hydrateTypeRelationships = (
  newTypes: CustomTypes,
  existingTypes: CustomTypes,
): CustomTypes => {
  newTypes.objects = newTypes.objects?.map((t) => {
    const existingType = existingTypes.objects?.find(
      (et) => et.name === t.name && et.relationships?.length,
    );
    if (!existingType) {
      return t;
    }

    return {
      ...t,
      relationships: existingType.relationships,
    };
  });

  return newTypes;
};
