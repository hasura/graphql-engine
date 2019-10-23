import gqlPattern from '../../Data/Common/GraphQLValidation';

// TODO (pull out default arguments, fields and types)
// TODO (check rare bug where description of an argument disappears)

export const defaultScalars = ['Int', 'String', 'Float', 'Boolean'];

const filterNameLessTypeLess = arr => {
  return arr.filter(item => !!item.name && item.type);
};

const reformOptionalArgs = args => {
  return args.map(a => {
    const arg = {
      ...a,
    };
    if (!arg.optional) {
      arg.type = `${arg.type}!`;
    }
    delete arg.optional;
    return arg;
  });
};

export const reformCustomTypes = typesFromState => {
  const sanitisedTypes = [];
  typesFromState.forEach(t => {
    if (!t.name) {
      return;
    }
    const sanitisedType = { ...t };
    if (t.fields) {
      sanitisedType.fields = filterNameLessTypeLess(t.fields);
    }
    if (t.arguments) {
      sanitisedType.arguments = filterNameLessTypeLess(t.arguments);
      sanitisedType.arguments = reformOptionalArgs(sanitisedType.arguments);
    }

    sanitisedTypes.push(sanitisedType);
  });

  const customTypes = {
    scalars: [],
    input_objects: [],
    objects: [],
    enums: [],
  };

  sanitisedTypes.forEach(_type => {
    const type = JSON.parse(JSON.stringify(_type));
    delete type.kind;
    switch (_type.kind) {
      case 'scalar':
        customTypes.scalars.push(type);
        return;
      case 'object':
        customTypes.objects.push(type);
        return;
      case 'input_object':
        customTypes.input_objects.push(type);
        return;
      default:
        return;
    }
  });

  return customTypes;
};

export const generateActionDefinition = ({
  arguments: args,
  outputType,
  kind = 'synchronous',
  webhook,
}) => {
  return {
    arguments: reformOptionalArgs(filterNameLessTypeLess(args)),
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
