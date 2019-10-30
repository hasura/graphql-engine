import { camelize } from 'inflection';

import gqlPattern from '../../Data/Common/GraphQLValidation';
import {
  filterNameLessTypeLess,
  filterNameless,
  filterValueLess,
} from '../../Types/utils';
import { wrapType, getTypenameMetadata } from '../../Types/wrappingTypeUtils';
import { isInputObjectType, isEnumType, isScalarType } from 'graphql';
import {
  gqlInbuiltTypes,
  defaultScalarType,
  defaultField,
} from './stateDefaults';

export const generateActionDefinition = ({
  arguments: args,
  outputType,
  kind = 'synchronous',
  webhook,
}) => {
  return {
    arguments: filterNameLessTypeLess(args),
    kind,
    output_type: outputType,
    webhook,
  };
};

export const getStateValidationError = ({ name, outputType, webhook }) => {
  if (!name) return 'Action name cannot be empty';
  if (!gqlPattern.test(name)) {
    return `"${name}" is not a GraphQL compatible name`;
  }

  if (!webhook) return 'Webhook cannot be empty';
  try {
    new URL(webhook); // eslint-disable-line
  } catch (e) {
    return 'Webhook must be a valid URL';
  }

  if (!outputType) return 'Please select an output type for the action';

  return null;
};

export const sanitiseState = state => {
  const newState = JSON.parse(JSON.stringify(state));
  newState.name = newState.name.trim();
  newState.webhook = newState.webhook.trim();
  newState.arguments = filterNameLessTypeLess(newState.arguments).map(a => {
    const argType = newState.types[a.type].name;
    const _arg = {
      ...a,
      type: wrapType(argType, a.typeWrap),
    };
    delete _arg.typeWrap;
    return _arg;
  });
  newState.outputType = newState.types[newState.outputType]
    ? newState.types[newState.outputType].name
    : '';
  newState.types = newState.types.map(t => {
    if (t.isInbuilt) return t;
    const _t = { ...t };

    switch (t.kind) {
      case 'scalar':
        return _t;

      case 'object':
        _t.fields = filterNameLessTypeLess(_t.fields).map(f => ({
          ...f,
          type: wrapType(newState.types[f.type].name, f.typeWrap),
        }));
        return _t;

      case 'input_object':
        console.log(newState.types);
        console.log(_t);
        _t.fields = filterNameLessTypeLess(_t.fields).map(f => ({
          ...f,
          type: wrapType(newState.types[f.type].name, f.typeWrap),
        }));
        return _t;

      case 'enum':
        _t.values = filterValueLess(_t.values).filter(v => !!v.value);
        return _t;
      default:
        return _t;
    }
  });
  return newState;
};

export const deriveExistingType = (
  currentTypename,
  actionTypes,
  selectedExistingType,
  existingTypemap
) => {
  const currentTypeIndex = actionTypes.findIndex(
    at => at.name === currentTypename
  );

  const namespaceTypename = name => {
    if (name === selectedExistingType) {
      return currentTypename;
    }
    return camelize(`${currentTypename}_${name}`);
  };

  const getTypeKind = graphqlType => {
    if (isInputObjectType(graphqlType)) return 'input_object';
    if (isEnumType(graphqlType)) return 'enum';
    if (isScalarType(graphqlType)) return 'scalar';
  };

  let newTypes = filterNameless(actionTypes);

  const chosenExistingType = existingTypemap[selectedExistingType];

  const usedTypenames = {};

  const isInbuiltType = _t => {
    return gqlInbuiltTypes.find(it => it.name === _t.name);
  };

  const generateTypes = (selectedType, enforcedTypename) => {
    if (isInbuiltType(selectedType)) return;

    const typeKind = getTypeKind(selectedType);

    const _tname = enforcedTypename || namespaceTypename(selectedType.name);
    if (usedTypenames[_tname]) return;
    usedTypenames[_tname] = true;
    const _t = {
      name: _tname,
      kind: typeKind,
    };

    if (typeKind !== 'input_object') {
      newTypes.push(_t);
      return;
    }

    _t.fields = [];

    Object.values(selectedType._fields).forEach(f => {
      const _f = {
        name: f.name,
      };
      const { index: typeWrap, typename } = getTypenameMetadata(f.type);
      generateTypes(existingTypemap[typename]);
      _f.typeWrap = typeWrap.toString();
      const namespacedTypename = isInbuiltType({ name: typename })
        ? typename
        : namespaceTypename(typename);
      _f.typename = namespacedTypename;
      _t.fields.push(_f);
    });

    _t.fields.push({
      ...defaultField,
    });

    if (enforcedTypename) {
      newTypes[currentTypeIndex] = _t;
    } else {
      newTypes.push(_t);
    }
  };

  generateTypes(chosenExistingType, currentTypename);

  newTypes = newTypes.map(nt => {
    if (nt.kind === 'input_object') {
      const newType = JSON.parse(JSON.stringify(nt));
      console.log(newType);
      newType.fields = newType.fields.map(f => {
        const _f = { ...f };
        _f.type = newTypes.findIndex(t => t.name === f.typename).toString();
        delete _f.typename;
        return _f;
      });
      return newType;
    }
    return nt;
  });

  return [...newTypes, defaultScalarType];
};
