import {
  getActionDefinitionFromSdl,
  getTypesFromSdl,
} from '../../../../shared/utils/sdlUtils';
import { Nullable } from '../../../Common/utils/tsUtils';

type ArgType = { name: string; type: string; description: string };

// Removes ! from type, and returns a new string
const getTrimmedType = (value: string): string => {
  const typeName =
    value[value.length - 1] === '!'
      ? value.substring(0, value.length - 1)
      : value;
  return typeName;
};

const getArgObjFromDefinition = (
  arg: ArgType,
  typesdef: Record<string, any>
): Nullable<Record<string, any>> => {
  let type = arg?.type;
  type = getTrimmedType(type);
  const name = arg?.name;
  if (type === 'String' || type === 'ID') return { [name]: `${name}` };
  if (type === 'Int' || type === 'Float' || type === 'BigInt')
    return { [name]: 10 };
  if (type === 'Boolean') return { [name]: false };
  if (type === '[String]' || type === '[ID]') {
    return { [name]: ['foo', 'bar'] };
  }
  if (type === '[Int]' || type === '[Float]' || type === '[BigInt]') {
    return { [name]: [10, 20] };
  }

  const userDefType = typesdef?.types.find(
    (t: Record<string, any>) => t.name === type
  );
  if (userDefType?.kind === 'input_object') {
    let obj = {};
    userDefType?.fields?.forEach((f: ArgType) => {
      obj = { ...obj, ...getArgObjFromDefinition(f, typesdef) };
    });
    return {
      [name]: obj,
    };
  }

  if (userDefType?.kind === 'enum') {
    return {
      [name]: userDefType.values?.[0]?.value ?? '',
    };
  }

  return {};
};

export const getActionRequestSampleInput = (
  actionSdl: string,
  typesSdl: string
): string => {
  const actionDef = getActionDefinitionFromSdl(actionSdl);
  const typesDef = getTypesFromSdl(typesSdl);
  let inputObj = {};

  // pass all top level args
  actionDef?.arguments?.forEach((arg: ArgType) => {
    inputObj = { ...inputObj, ...getArgObjFromDefinition(arg, typesDef) };
  });

  const obj = {
    action: {
      name: actionDef?.name,
    },
    input: {
      ...inputObj,
    },
  };

  const value = JSON.stringify(obj, null, 2);
  return value;
};
