import { camelize } from 'inflection';
import { filterNameLessTypeLess, filterNameless } from '../../Types/utils';
import { getTypenameMetadata } from '../../Types/wrappingTypeUtils';
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

export const getStateValidationError = ({ webhook }) => {
  if (!webhook) return 'Webhook cannot be empty';
  try {
    new URL(webhook); // eslint-disable-line
  } catch (e) {
    return 'Webhook must be a valid URL';
  }
  return null;
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

    if (typeKind === 'enum') {
      newTypes.push({
        ..._t,
        values: selectedType._values,
      });
      return;
    }

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
