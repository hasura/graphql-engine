import {
  getActionDefinitionFromSdl,
  getTypesFromSdl,
} from '../../../../shared/utils/sdlUtils';
import {
  filterNameLessTypeLess,
  FlattenCustomType,
  inbuiltTypes,
} from '../../../../shared/utils/hasuraCustomTypeUtils';
import { unwrapType } from '../../../../shared/utils/wrappingTypeUtils';
import {
  isValidTemplateLiteral,
  transformHeaderConfigs,
} from '@hasura/shared/utils';
import { Action, ActionDefinition } from '@hasura/shared/types';
import { findType, getActionArguments, getActionOutputType } from '../../utils';

export const isInbuiltType = (typename) => {
  return !!inbuiltTypes[typename];
};

export const generateActionDefinition = (
  {
    arguments: args,
    outputType,
    kind = 'synchronous' as 'synchronous' | 'asynchronous',
    handler,
    actionType,
    headers,
    forwardClientHeaders,
    timeout,
  },
  requestTransform,
  responseTransform,
): ActionDefinition => {
  return {
    arguments: filterNameLessTypeLess(args),
    kind,
    output_type: outputType,
    handler,
    type: actionType,
    headers: transformHeaderConfigs(headers),
    forward_client_headers: forwardClientHeaders,
    timeout,
    request_transform: requestTransform ?? null,
    response_transform: responseTransform ?? null,
  };
};

export const getStateValidationError = ({ handler }) => {
  if (!handler) return 'Handler cannot be empty';
  if (isValidTemplateLiteral(handler)) return null;
  try {
    new URL(handler);
  } catch (e) {
    return 'Handler must be a valid URL or a template';
  }
  return null;
};

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
  typesdef: Record<string, any>,
): Record<string, any> => {
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
    (t: Record<string, any>) => t.name === type,
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
  typesSdl: string,
) => {
  const actionDef = getActionDefinitionFromSdl(actionSdl);
  const typesDef = getTypesFromSdl(typesSdl);
  let inputObj = {};

  // pass all top level args
  actionDef?.arguments?.forEach((arg: ArgType) => {
    inputObj = { ...inputObj, ...getArgObjFromDefinition(arg, typesDef) };
  });

  return {
    action: {
      name: actionDef?.name,
    },
    input: {
      ...inputObj,
    },
  };
};

export const getActionTypes = (
  currentAction: Action,
  allTypes: FlattenCustomType[],
): FlattenCustomType[] => {
  const actionTypes = {};
  const actionArgs = getActionArguments(currentAction);
  const actionOutputType = getActionOutputType(currentAction);

  const getDependentTypes = (maybeWrappedTypename: string) => {
    const { typename } = unwrapType(maybeWrappedTypename);
    if (isInbuiltType(typename)) return;
    if (actionTypes[typename]) return;

    const type = findType(allTypes, typename);
    if (!type) {
      return;
    }

    actionTypes[typename] = type;

    if (
      (type?.kind === 'input_objects' || type?.kind === 'objects') &&
      type.definition?.fields?.length
    ) {
      type.definition.fields.forEach((f) => {
        getDependentTypes(f.type);
        if ((f as any).arguments) {
          (f as any).arguments.forEach((a) => {
            getDependentTypes(a.type);
          });
        }
      });
    }
  };

  if (actionArgs.length) {
    actionArgs.forEach((a) => {
      getDependentTypes(a.type);
    });
  }

  getDependentTypes(actionOutputType);

  return Object.values(actionTypes);
};
