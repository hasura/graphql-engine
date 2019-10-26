import gqlPattern from '../../Data/Common/GraphQLValidation';
import { filterNameLessTypeLess } from '../../Types/utils';

// TODO (pull out default arguments, fields and types)
// TODO (check rare bug where description of an argument disappears)

// const reformOptionalArgs = args => {
//   return args.map(a => {
//     const arg = {
//       ...a,
//     };
//     if (!arg.optional) {
//       arg.type = `${arg.type}!`;
//     }
//     delete arg.optional;
//     return arg;
//   });
// };

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
    return {
      ...a,
      type: newState.types[a.type].name,
    };
  });
  newState.outputType = newState.types[newState.outputType]
    ? newState.types[newState.outputType].name
    : '';
  newState.types = newState.types.map(t => {
    if (t.isInbuilt) return t;
    const _t = { ...t };
    console.log(_t);

    switch (t.kind) {
      case 'scalar':
        return _t;

      case 'object':
        _t.arguments = filterNameLessTypeLess(_t.arguments).map(a => ({
          ...a,
          type: newState.types[a.type].name,
        }));
        _t.fields = filterNameLessTypeLess(_t.fields).map(f => ({
          ...f,
          type: newState.types[f.type].name,
        }));
        return _t;

      case 'input_object':
        _t.fields = filterNameLessTypeLess(_t.fields).map(f => ({
          ...f,
          type: newState.types[f.type].name,
        }));
        return _t;

      case 'enum':
        _t.values = _t.values.filter(v => !!v.value);
        return _t;
      default:
        return _t;
    }
  });
  return newState;
};
