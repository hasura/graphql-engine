/* eslint-disable */
import React from 'react';

import {
  getNamedType,
  GraphQLObjectType,
  isEnumType,
  isInputObjectType,
  isInterfaceType,
  isLeafType,
  isNonNullType,
  isObjectType,
  isRequiredInputField,
  isScalarType,
  isUnionType,
  isWrappingType,
  parse,
  print,
} from 'graphql';

function capitalize(string) {
  return string.charAt(0).toUpperCase() + string.slice(1);
}

const graphiqlArrowOpen = (
  <svg width="12" height="9">
    <path fill="#666" d="M 0 2 L 9 2 L 4.5 7.5 z" />
  </svg>
);

const graphiqlArrowClosed = (
  <svg width="12" height="9">
    <path fill="#666" d="M 0 0 L 0 9 L 5.5 4.5 z" />
  </svg>
);

const checkboxChecked = (
  <svg
    style={{ marginRight: '3px', marginLeft: '-3px' }}
    width="12"
    height="12"
    viewBox="0 0 18 18"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="M16 0H2C0.9 0 0 0.9 0 2V16C0 17.1 0.9 18 2 18H16C17.1 18 18 17.1 18 16V2C18 0.9 17.1 0 16 0ZM16 16H2V2H16V16ZM14.99 6L13.58 4.58L6.99 11.17L4.41 8.6L2.99 10.01L6.99 14L14.99 6Z"
      fill="#666"
    />
  </svg>
);

const checkboxEmpty = (
  <svg
    style={{ marginRight: '3px', marginLeft: '-3px' }}
    width="12"
    height="12"
    viewBox="0 0 18 18"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="M16 2V16H2V2H16ZM16 0H2C0.9 0 0 0.9 0 2V16C0 17.1 0.9 18 2 18H16C17.1 18 18 17.1 18 16V2C18 0.9 17.1 0 16 0Z"
      fill="#CCC"
    />
  </svg>
);

function Checkbox(props) {
  return props.checked ? checkboxChecked : checkboxEmpty;
}

function defaultGetDefaultFieldNames(type) {
  const fields = type.getFields();

  // Is there an `id` field?
  if (fields.id) {
    const res = ['id'];
    if (fields.email) {
      res.push('email');
    } else if (fields.name) {
      res.push('name');
    }
    return res;
  }

  // Is there an `edges` field?
  if (fields.edges) {
    return ['edges'];
  }

  // Is there an `node` field?
  if (fields.node) {
    return ['node'];
  }

  if (fields.nodes) {
    return ['nodes'];
  }

  // Include all leaf-type fields.
  const leafFieldNames = [];
  Object.keys(fields).forEach(fieldName => {
    if (isLeafType(fields[fieldName].type)) {
      leafFieldNames.push(fieldName);
    }
  });
  return leafFieldNames.slice(0, 2); // Prevent too many fields from being added
}

function isRequiredArgument(arg) {
  return isNonNullType(arg.type) && arg.defaultValue === undefined;
}

function unwrapOutputType(outputType) {
  let unwrappedType = outputType;
  while (isWrappingType(unwrappedType)) {
    unwrappedType = unwrappedType.ofType;
  }
  return unwrappedType;
}

function unwrapInputType(inputType) {
  let unwrappedType = inputType;
  while (isWrappingType(unwrappedType)) {
    unwrappedType = unwrappedType.ofType;
  }
  return unwrappedType;
}

function coerceArgValue(argType, value) {
  if (isScalarType(argType)) {
    try {
      switch (argType.name) {
        case 'String':
          return {
            kind: 'StringValue',
            value: String(argType.parseValue(value)),
          };
        case 'Float':
          return {
            kind: 'FloatValue',
            value: String(argType.parseValue(parseFloat(value))),
          };
        case 'Int':
          return {
            kind: 'IntValue',
            value: String(argType.parseValue(parseInt(value, 10))),
          };
        case 'Boolean':
          try {
            const parsed = JSON.parse(value);
            if (typeof parsed === 'boolean') {
              return { kind: 'BooleanValue', value: parsed };
            }
            return { kind: 'BooleanValue', value: false };
          } catch (e) {
            return {
              kind: 'BooleanValue',
              value: false,
            };
          }
        default:
          return {
            kind: 'StringValue',
            value: String(argType.parseValue(value)),
          };
      }
    } catch (e) {
      console.error('error coercing arg value', e, value);
      return { kind: 'StringValue', value: value };
    }
  } else {
    try {
      const parsedValue = argType.parseValue(value);
      if (parsedValue) {
        return { kind: 'EnumValue', value: String(parsedValue) };
      }
      return { kind: 'EnumValue', value: argType.getValues()[0].name };
    } catch (e) {
      return { kind: 'EnumValue', value: argType.getValues()[0].name };
    }
  }
}

class InputArgView extends React.PureComponent {
  _getArgSelection = () => {
    return this.props.selection.fields.find(
      field => field.name.value === this.props.arg.name
    );
  };

  _removeArg = () => {
    const { selection } = this.props;
    const argSelection = this._getArgSelection();
    this._previousArgSelection = argSelection;
    this.props.modifyFields(
      selection.fields.filter(field => field !== argSelection)
    );
  };

  _addArg = () => {
    const {
      selection,
      arg,
      getDefaultScalarArgValue,
      parentField,
      makeDefaultArg,
    } = this.props;
    const argType = unwrapInputType(arg.type);

    let argSelection = null;
    if (this._previousArgSelection) {
      argSelection = this._previousArgSelection;
    } else if (isInputObjectType(argType)) {
      const fields = argType.getFields();
      argSelection = {
        kind: 'ObjectField',
        name: { kind: 'Name', value: arg.name },
        value: {
          kind: 'ObjectValue',
          fields: defaultInputObjectFields(
            getDefaultScalarArgValue,
            makeDefaultArg,
            parentField,
            Object.keys(fields).map(k => fields[k])
          ),
        },
      };
    } else if (isLeafType(argType)) {
      argSelection = {
        kind: 'ObjectField',
        name: { kind: 'Name', value: arg.name },
        value: getDefaultScalarArgValue(parentField, arg, argType),
      };
    }

    if (!argSelection) {
      console.error('Unable to add arg for argType', argType);
    } else {
      this.props.modifyFields([...(selection.fields || []), argSelection]);
    }
  };

  _setArgValue = event => {
    const { selection } = this.props;
    const argSelection = this._getArgSelection();
    if (!argSelection) {
      console.error('missing arg selection when setting arg value');
      return;
    }
    const argType = unwrapInputType(this.props.arg.type);
    if (!isLeafType(argType)) {
      console.warn('Unable to handle non leaf types in setArgValue');
      return;
    }
    const targetValue = event.target.value;

    this.props.modifyFields(
      (selection.fields || []).map(field =>
        field === argSelection
          ? {
              ...field,
              value: coerceArgValue(argType, targetValue),
            }
          : field
      )
    );
  };

  _modifyChildFields = fields => {
    this.props.modifyFields(
      this.props.selection.fields.map(field =>
        field.name.value === this.props.arg.name
          ? {
              ...field,
              value: {
                kind: 'ObjectValue',
                fields: fields,
              },
            }
          : field
      )
    );
  };

  render() {
    const { arg, parentField } = this.props;
    const argSelection = this._getArgSelection();

    return (
      <AbstractArgView
        argValue={argSelection ? argSelection.value : null}
        arg={arg}
        parentField={parentField}
        addArg={this._addArg}
        removeArg={this._removeArg}
        setArgFields={this._modifyChildFields}
        setArgValue={this._setArgValue}
        getDefaultScalarArgValue={this.props.getDefaultScalarArgValue}
        makeDefaultArg={this.props.makeDefaultArg}
        onRunOperation={this.props.onRunOperation}
      />
    );
  }
}

export function defaultValue(argType) {
  if (isEnumType(argType)) {
    return { kind: 'EnumValue', value: argType.getValues()[0].name };
  }
  switch (argType.name) {
    case 'String':
      return { kind: 'StringValue', value: '' };
    case 'Float':
      return { kind: 'FloatValue', value: '1.5' };
    case 'Int':
      return { kind: 'IntValue', value: '10' };
    case 'Boolean':
      return { kind: 'BooleanValue', value: false };
    default:
      return { kind: 'StringValue', value: '' };
  }
}

function defaultGetDefaultScalarArgValue(parentField, arg, argType) {
  return defaultValue(argType);
}

class ArgView extends React.PureComponent {
  _getArgSelection = () => {
    const { selection } = this.props;

    return (selection.arguments || []).find(
      arg => arg.name.value === this.props.arg.name
    );
  };
  _removeArg = () => {
    const { selection } = this.props;
    const argSelection = this._getArgSelection();
    this._previousArgSelection = argSelection;
    this.props.modifyArguments(
      (selection.arguments || []).filter(arg => arg !== argSelection)
    );
  };
  _addArg = () => {
    const {
      selection,
      getDefaultScalarArgValue,
      makeDefaultArg,
      parentField,
      arg,
    } = this.props;
    const argType = unwrapInputType(arg.type);

    let argSelection = null;
    if (this._previousArgSelection) {
      argSelection = this._previousArgSelection;
    } else if (isInputObjectType(argType)) {
      const fields = argType.getFields();
      argSelection = {
        kind: 'Argument',
        name: { kind: 'Name', value: arg.name },
        value: {
          kind: 'ObjectValue',
          fields: defaultInputObjectFields(
            getDefaultScalarArgValue,
            makeDefaultArg,
            parentField,
            Object.keys(fields).map(k => fields[k])
          ),
        },
      };
    } else if (isLeafType(argType)) {
      argSelection = {
        kind: 'Argument',
        name: { kind: 'Name', value: arg.name },
        value: getDefaultScalarArgValue(parentField, arg, argType),
      };
    }

    if (!argSelection) {
      console.error('Unable to add arg for argType', argType);
    } else {
      this.props.modifyArguments([
        ...(selection.arguments || []),
        argSelection,
      ]);
    }
  };
  _setArgValue = event => {
    const { selection } = this.props;
    const argSelection = this._getArgSelection();
    if (!argSelection) {
      console.error('missing arg selection when setting arg value');
      return;
    }
    const argType = unwrapInputType(this.props.arg.type);
    if (!isLeafType(argType)) {
      console.warn('Unable to handle non leaf types in setArgValue');
      return;
    }

    const targetValue = event.target.value;

    this.props.modifyArguments(
      (selection.arguments || []).map(a =>
        a === argSelection
          ? {
              ...a,
              value: coerceArgValue(argType, targetValue),
            }
          : a
      )
    );
  };

  _setArgFields = fields => {
    const { selection } = this.props;
    const argSelection = this._getArgSelection();
    if (!argSelection) {
      console.error('missing arg selection when setting arg value');
      return;
    }

    this.props.modifyArguments(
      (selection.arguments || []).map(a =>
        a === argSelection
          ? {
              ...a,
              value: {
                kind: 'ObjectValue',
                fields,
              },
            }
          : a
      )
    );
  };

  render() {
    const { arg, parentField } = this.props;
    const argSelection = this._getArgSelection();

    return (
      <AbstractArgView
        argValue={argSelection ? argSelection.value : null}
        arg={arg}
        parentField={parentField}
        addArg={this._addArg}
        removeArg={this._removeArg}
        setArgFields={this._setArgFields}
        setArgValue={this._setArgValue}
        getDefaultScalarArgValue={this.props.getDefaultScalarArgValue}
        makeDefaultArg={this.props.makeDefaultArg}
        onRunOperation={this.props.onRunOperation}
      />
    );
  }
}

function isRunShortcut(event) {
  return event.metaKey && event.key === 'Enter';
}

class ScalarInput extends React.PureComponent {
  _handleChange = event => {
    this.props.setArgValue(event);
  };

  componentDidMount() {
    const input = this._ref;
    const activeElement = document.activeElement;
    if (
      input &&
      activeElement &&
      !(activeElement instanceof HTMLTextAreaElement)
    ) {
      input.focus();
      input.setSelectionRange(0, input.value.length);
    }
  }

  render() {
    const { arg, argValue } = this.props;
    const argType = unwrapInputType(arg.type);
    const color =
      this.props.argValue.kind === 'StringValue' ? '#D64292' : '#2882F9';
    const value = typeof argValue.value === 'string' ? argValue.value : '';
    return (
      <span style={{ color }}>
        {argType.name === 'String' ? '"' : ''}
        <input
          style={{
            border: 'none',
            borderBottom: '1px solid #888',
            outline: 'none',
            color,
            width: `${Math.max(1, value.length)}ch`,
          }}
          ref={ref => {
            this._ref = ref;
          }}
          type="text"
          onKeyDown={event => {
            if (isRunShortcut(event)) {
              this.props.onRunOperation(event);
            }
          }}
          onChange={this._handleChange}
          value={value}
        />
        {argType.name === 'String' ? '"' : ''}
      </span>
    );
  }
}

class AbstractArgView extends React.PureComponent {
  render() {
    const { argValue, arg } = this.props;
    /* TODO: handle List types*/
    const argType = unwrapInputType(arg.type);

    let input = null;
    if (argValue) {
      if (argValue.kind === 'Variable') {
        input = (
          <span style={{ color: '#397D13' }}>${argValue.name.value}</span>
        );
      } else if (isScalarType(argType)) {
        if (argType.name === 'Boolean') {
          input = (
            <select
              style={{ backgroundColor: 'white', color: '#D47509' }}
              onChange={this.props.setArgValue}
              value={
                argValue.kind === 'BooleanValue' ? argValue.value : undefined
              }
            >
              <option key="true" value="true">
                true
              </option>
              <option key="false" value="false">
                false
              </option>
            </select>
          );
        } else {
          input = (
            <ScalarInput
              setArgValue={this.props.setArgValue}
              arg={arg}
              argValue={argValue}
              onRunOperation={this.props.onRunOperation}
            />
          );
        }
      } else if (isEnumType(argType)) {
        if (argValue.kind === 'EnumValue') {
          input = (
            <select
              style={{ backgroundColor: 'white', color: '#0B7FC7' }}
              onChange={this.props.setArgValue}
              value={argValue.value}
            >
              {argType.getValues().map(value => (
                <option key={value.name} value={value.name}>
                  {value.name}
                </option>
              ))}
            </select>
          );
        } else {
          console.error(
            'arg mismatch between arg and selection',
            argType,
            argValue
          );
        }
      } else if (isInputObjectType(argType)) {
        if (argValue.kind === 'ObjectValue') {
          const fields = argType.getFields();
          input = (
            <div style={{ marginLeft: 16 }}>
              {Object.keys(fields)
                .sort()
                .map(fieldName => (
                  <InputArgView
                    key={fieldName}
                    arg={fields[fieldName]}
                    parentField={this.props.parentField}
                    selection={argValue}
                    modifyFields={this.props.setArgFields}
                    getDefaultScalarArgValue={
                      this.props.getDefaultScalarArgValue
                    }
                    makeDefaultArg={this.props.makeDefaultArg}
                    onRunOperation={this.props.onRunOperation}
                  />
                ))}
            </div>
          );
        } else {
          console.error(
            'arg mismatch between arg and selection',
            argType,
            argValue
          );
        }
      }
    }

    return (
      <div
        style={{
          cursor: 'pointer',
          minHeight: '16px',
          WebkitUserSelect: 'none',
          userSelect: 'none',
        }}
        data-arg-name={arg.name}
        data-arg-type={argType.name}
      >
        <span
          style={{ cursor: 'pointer' }}
          onClick={argValue ? this.props.removeArg : this.props.addArg}
        >
          <Checkbox checked={!!argValue} />
          <span title={arg.description} style={{ color: '#8B2BB9' }}>
            {arg.name}
            {isRequiredArgument(arg) ? '*' : ''}:
          </span>
        </span>{' '}
        {input || <span />}
      </div>
    );
  }
}

class AbstractView extends React.PureComponent {
  _addFragment = () => {
    this.props.modifySelections([
      ...this.props.selections,
      this._previousSelection || {
        kind: 'InlineFragment',
        typeCondition: {
          kind: 'NamedType',
          name: { kind: 'Name', value: this.props.implementingType.name },
        },
        selectionSet: {
          kind: 'SelectionSet',
          selections: this.props
            .getDefaultFieldNames(this.props.implementingType)
            .map(fieldName => ({
              kind: 'Field',
              name: { kind: 'Name', value: fieldName },
            })),
        },
      },
    ]);
  };
  _removeFragment = () => {
    const thisSelection = this._getSelection();
    this._previousSelection = thisSelection;
    this.props.modifySelections(
      this.props.selections.filter(s => s !== thisSelection)
    );
  };
  _getSelection = () => {
    const selection = this.props.selections.find(
      selection =>
        selection.kind === 'InlineFragment' &&
        selection.typeCondition &&
        this.props.implementingType.name === selection.typeCondition.name.value
    );
    if (!selection) {
      return null;
    }
    if (selection.kind === 'InlineFragment') {
      return selection;
    }
  };

  _modifyChildSelections = selections => {
    const thisSelection = this._getSelection();
    this.props.modifySelections(
      this.props.selections.map(selection => {
        if (selection === thisSelection) {
          return {
            directives: selection.directives,
            kind: 'InlineFragment',
            typeCondition: {
              kind: 'NamedType',
              name: { kind: 'Name', value: this.props.implementingType.name },
            },
            selectionSet: {
              kind: 'SelectionSet',
              selections,
            },
          };
        }
        return selection;
      })
    );
  };

  render() {
    const { implementingType, schema, getDefaultFieldNames } = this.props;
    const selection = this._getSelection();
    const fields = implementingType.getFields();
    const childSelections = selection
      ? selection.selectionSet
        ? selection.selectionSet.selections
        : []
      : [];
    return (
      <div>
        <span
          style={{ cursor: 'pointer' }}
          onClick={selection ? this._removeFragment : this._addFragment}
        >
          <Checkbox checked={!!selection} />
          <span style={{ color: '#CA9800' }}>
            {this.props.implementingType.name}
          </span>
        </span>
        {selection ? (
          <div style={{ marginLeft: 16 }}>
            {Object.keys(fields)
              .sort()
              .map(fieldName => (
                <FieldView
                  key={fieldName}
                  field={fields[fieldName]}
                  selections={childSelections}
                  modifySelections={this._modifyChildSelections}
                  schema={schema}
                  getDefaultFieldNames={getDefaultFieldNames}
                  getDefaultScalarArgValue={this.props.getDefaultScalarArgValue}
                  makeDefaultArg={this.props.makeDefaultArg}
                  onRunOperation={this.props.onRunOperation}
                />
              ))}
          </div>
        ) : null}
      </div>
    );
  }
}

function defaultInputObjectFields(
  getDefaultScalarArgValue,
  makeDefaultArg,
  parentField,
  fields
) {
  const nodes = [];
  for (const field of fields) {
    if (
      isRequiredInputField(field) ||
      (makeDefaultArg && makeDefaultArg(parentField, field))
    ) {
      const fieldType = unwrapInputType(field.type);
      if (isInputObjectType(fieldType)) {
        const fields = fieldType.getFields();
        nodes.push({
          kind: 'ObjectField',
          name: { kind: 'Name', value: field.name },
          value: {
            kind: 'ObjectValue',
            fields: defaultInputObjectFields(
              getDefaultScalarArgValue,
              makeDefaultArg,
              parentField,
              Object.keys(fields).map(k => fields[k])
            ),
          },
        });
      } else if (isLeafType(fieldType)) {
        nodes.push({
          kind: 'ObjectField',
          name: { kind: 'Name', value: field.name },
          value: getDefaultScalarArgValue(parentField, field, fieldType),
        });
      }
    }
  }
  return nodes;
}

function defaultArgs(getDefaultScalarArgValue, makeDefaultArg, field) {
  const args = [];
  for (const arg of field.args) {
    if (
      isRequiredArgument(arg) ||
      (makeDefaultArg && makeDefaultArg(field, arg))
    ) {
      const argType = unwrapInputType(arg.type);
      if (isInputObjectType(argType)) {
        const fields = argType.getFields();
        args.push({
          kind: 'Argument',
          name: { kind: 'Name', value: arg.name },
          value: {
            kind: 'ObjectValue',
            fields: defaultInputObjectFields(
              getDefaultScalarArgValue,
              makeDefaultArg,
              field,
              Object.keys(fields).map(k => fields[k])
            ),
          },
        });
      } else if (isLeafType(argType)) {
        args.push({
          kind: 'Argument',
          name: { kind: 'Name', value: arg.name },
          value: getDefaultScalarArgValue(field, arg, argType),
        });
      }
    }
  }
  return args;
}

class FieldView extends React.PureComponent {
  _addAllFieldsToSelections = rawSubfields => {
    const subFields = rawSubfields
      ? Object.keys(rawSubfields).map(fieldName => {
          return {
            kind: 'Field',
            name: { kind: 'Name', value: fieldName },
            arguments: [],
          };
        })
      : [];

    const subSelectionSet = {
      kind: 'SelectionSet',
      selections: subFields,
    };

    const nextSelections = [
      ...this.props.selections.filter(selection => {
        if (selection.kind === 'InlineFragment') {
          return true;
        }
        // Remove the current selection set for the target field
        return selection.name.value !== this.props.field.name;
      }),
      {
        kind: 'Field',
        name: { kind: 'Name', value: this.props.field.name },
        arguments: defaultArgs(
          this.props.getDefaultScalarArgValue,
          this.props.makeDefaultArg,
          this.props.field
        ),
        selectionSet: subSelectionSet,
      },
    ];

    this.props.modifySelections(nextSelections);
  };

  _addFieldToSelections = rawSubfields => {
    const nextSelections = [
      ...this.props.selections,
      this._previousSelection || {
        kind: 'Field',
        name: { kind: 'Name', value: this.props.field.name },
        arguments: defaultArgs(
          this.props.getDefaultScalarArgValue,
          this.props.makeDefaultArg,
          this.props.field
        ),
      },
    ];

    this.props.modifySelections(nextSelections);
  };

  _handleUpdateSelections = event => {
    const selection = this._getSelection();
    if (selection && !event.altKey) {
      this._removeFieldFromSelections();
    } else {
      const fieldType = getNamedType(this.props.field.type);
      const rawSubfields = isObjectType(fieldType) && fieldType.getFields();

      const shouldSelectAllSubfields = !!rawSubfields && event.altKey;

      shouldSelectAllSubfields
        ? this._addAllFieldsToSelections(rawSubfields)
        : this._addFieldToSelections(rawSubfields);
    }
  };

  _removeFieldFromSelections = () => {
    const previousSelection = this._getSelection();
    this._previousSelection = previousSelection;
    this.props.modifySelections(
      this.props.selections.filter(selection => selection !== previousSelection)
    );
  };
  _getSelection = () => {
    const selection = this.props.selections.find(
      selection =>
        selection.kind === 'Field' &&
        this.props.field.name === selection.name.value
    );
    if (!selection) {
      return null;
    }
    if (selection.kind === 'Field') {
      return selection;
    }
  };

  _setArguments = argumentNodes => {
    const selection = this._getSelection();
    if (!selection) {
      console.error('Missing selection when setting arguments', argumentNodes);
      return;
    }
    this.props.modifySelections(
      this.props.selections.map(s =>
        s === selection
          ? {
              alias: selection.alias,
              arguments: argumentNodes,
              directives: selection.directives,
              kind: 'Field',
              name: selection.name,
              selectionSet: selection.selectionSet,
            }
          : s
      )
    );
  };

  _modifyChildSelections = selections => {
    this.props.modifySelections(
      this.props.selections.map(selection => {
        if (
          selection.kind === 'Field' &&
          this.props.field.name === selection.name.value
        ) {
          if (selection.kind !== 'Field') {
            throw new Error('invalid selection');
          }
          return {
            alias: selection.alias,
            arguments: selection.arguments,
            directives: selection.directives,
            kind: 'Field',
            name: selection.name,
            selectionSet: {
              kind: 'SelectionSet',
              selections,
            },
          };
        }
        return selection;
      })
    );
  };

  render() {
    const { field, schema, getDefaultFieldNames } = this.props;
    const selection = this._getSelection();
    const type = unwrapOutputType(field.type);
    const args = field.args.sort((a, b) => a.name.localeCompare(b.name));
    const node = (
      <div className="graphiql-explorer-node">
        <span
          title={field.description}
          style={{
            cursor: 'pointer',
            display: 'inline-flex',
            alignItems: 'center',
            minHeight: '16px',
            WebkitUserSelect: 'none',
            userSelect: 'none',
          }}
          data-field-name={field.name}
          data-field-type={type.name}
          onClick={this._handleUpdateSelections}
        >
          {isObjectType(type) ? (
            <span>{selection ? graphiqlArrowOpen : graphiqlArrowClosed}</span>
          ) : null}
          {isObjectType(type) ? null : <Checkbox checked={!!selection} />}
          <span style={{ color: 'rgb(31, 97, 160)' }}>{field.name}</span>
        </span>
        {selection && args.length ? (
          <div style={{ marginLeft: 16 }}>
            {args.map(arg => (
              <ArgView
                key={arg.name}
                parentField={field}
                arg={arg}
                selection={selection}
                modifyArguments={this._setArguments}
                getDefaultScalarArgValue={this.props.getDefaultScalarArgValue}
                makeDefaultArg={this.props.makeDefaultArg}
                onRunOperation={this.props.onRunOperation}
              />
            ))}
          </div>
        ) : null}
      </div>
    );

    if (
      selection &&
      (isObjectType(type) || isInterfaceType(type) || isUnionType(type))
    ) {
      const fields = isUnionType(type) ? {} : type.getFields();
      const childSelections = selection
        ? selection.selectionSet
          ? selection.selectionSet.selections
          : []
        : [];
      return (
        <div>
          {node}
          <div style={{ marginLeft: 16 }}>
            {Object.keys(fields)
              .sort()
              .map(fieldName => (
                <FieldView
                  key={fieldName}
                  field={fields[fieldName]}
                  selections={childSelections}
                  modifySelections={this._modifyChildSelections}
                  schema={schema}
                  getDefaultFieldNames={getDefaultFieldNames}
                  getDefaultScalarArgValue={this.props.getDefaultScalarArgValue}
                  makeDefaultArg={this.props.makeDefaultArg}
                  onRunOperation={this.props.onRunOperation}
                />
              ))}
            {isInterfaceType(type) || isUnionType(type)
              ? schema
                  .getPossibleTypes(type)
                  .map(type => (
                    <AbstractView
                      key={type.name}
                      implementingType={type}
                      selections={childSelections}
                      modifySelections={this._modifyChildSelections}
                      schema={schema}
                      getDefaultFieldNames={getDefaultFieldNames}
                      getDefaultScalarArgValue={
                        this.props.getDefaultScalarArgValue
                      }
                      makeDefaultArg={this.props.makeDefaultArg}
                      onRunOperation={this.props.onRunOperation}
                    />
                  ))
              : null}
          </div>
        </div>
      );
    }
    return node;
  }
}

function parseQuery(text) {
  try {
    if (!text.trim()) {
      return null;
    }
    return parse(
      text,
      // Tell graphql to not bother track locations when parsing, we don't need
      // it and it's a tiny bit more expensive.
      { noLocation: true }
    );
  } catch (e) {
    return new Error(e);
  }
}

const DEFAULT_OPERATION = {
  kind: 'OperationDefinition',
  operation: 'query',
  variableDefinitions: [],
  name: { kind: 'Name', value: 'MyQuery' },
  directives: [],
  selectionSet: {
    kind: 'SelectionSet',
    selections: [],
  },
};
const DEFAULT_DOCUMENT = {
  kind: 'Document',
  definitions: [DEFAULT_OPERATION],
};
let parseQueryMemoize = null;
function memoizeParseQuery(query) {
  if (parseQueryMemoize && parseQueryMemoize[0] === query) {
    return parseQueryMemoize[1];
  }
  const result = parseQuery(query);
  if (!result) {
    return DEFAULT_DOCUMENT;
  } else if (result instanceof Error) {
    if (parseQueryMemoize) {
      // Most likely a temporarily invalid query while they type
      return parseQueryMemoize[1];
    }
    return DEFAULT_DOCUMENT;
  }
  parseQueryMemoize = [query, result];
  return result;
}

const buttonStyle = {
  fontSize: '1.2em',
  padding: '0px',
  backgroundColor: 'white',
  border: 'none',
  margin: '5px 0px',
  height: '40px',
  width: '100%',
  display: 'block',
  maxWidth: 'none',
};

const explorerActionsStyle = {
  margin: '4px -8px -8px',
  paddingLeft: '8px',
  bottom: '0px',
  width: '100%',
  textAlign: 'center',
  background: 'none',
  borderTop: 'none',
  borderBottom: 'none',
};

class RootView extends React.PureComponent {
  _modifySelections = selections => {
    let operationDef = this.props.definition;

    if (
      operationDef.selectionSet.selections.length === 0 &&
      this._previousOperationDef
    ) {
      operationDef = this._previousOperationDef;
    }

    let newOperationDef;

    if (selections.length === 0) {
      this._previousOperationDef = operationDef;
      newOperationDef = null;
    } else if (operationDef.kind === 'FragmentDefinition') {
      newOperationDef = {
        ...operationDef,
        selectionSet: {
          ...operationDef.selectionSet,
          selections,
        },
      };
    } else if (operationDef.kind === 'OperationDefinition') {
      newOperationDef = {
        ...operationDef,
        selectionSet: {
          ...operationDef.selectionSet,
          selections,
        },
      };
    }

    this.props.onEdit(newOperationDef);
  };

  _onOperationRename = event =>
    this.props.onOperationRename(event.target.value);

  _handlePotentialRun = event => {
    if (isRunShortcut(event)) {
      this.props.onRunOperation(this.props.name);
    }
  };

  render() {
    const {
      fields,
      operation,
      name,
      definition,
      schema,
      getDefaultFieldNames,
    } = this.props;
    const operationDef = definition;
    const selections = operationDef.selectionSet.selections;

    const operationDisplayName =
      this.props.name || `${capitalize(operation)} Name`;

    return (
      <div
        id={`${operation}-${name || 'unknown'}`}
        style={{
          borderBottom: '1px solid #d6d6d6',
          marginBottom: '0em',
          paddingBottom: '1em',
        }}
      >
        <div style={{ color: '#B11A04', paddingBottom: 4 }}>
          {operation}{' '}
          <span style={{ color: 'rgb(193, 42,80)' }}>
            <input
              style={{
                border: 'none',
                borderBottom: '1px solid #888',
                outline: 'none',
                color: 'rgb(193, 42,80)',
                width: `${Math.max(4, operationDisplayName.length)}ch`,
              }}
              autoComplete="false"
              placeholder={`${capitalize(operation)} Name`}
              value={this.props.name}
              onKeyDown={this._handlePotentialRun}
              onChange={this._onOperationRename}
            />
          </span>
          {this.props.onTypeName ? (
            <span>
              <br />
              {`on ${this.props.onTypeName}`}
            </span>
          ) : (
            ''
          )}
        </div>

        {Object.keys(fields || {})
          .sort()
          .map(fieldName => (
            <FieldView
              key={fieldName}
              field={fields[fieldName]}
              selections={selections}
              modifySelections={this._modifySelections}
              schema={schema}
              getDefaultFieldNames={getDefaultFieldNames}
              getDefaultScalarArgValue={this.props.getDefaultScalarArgValue}
              makeDefaultArg={this.props.makeDefaultArg}
              onRunOperation={this.props.onRunOperation}
            />
          ))}
      </div>
    );
  }
}

class Explorer extends React.PureComponent {
  static defaultProps = {
    getDefaultFieldNames: defaultGetDefaultFieldNames,
    getDefaultScalarArgValue: defaultGetDefaultScalarArgValue,
  };

  _resetScroll = () => {
    const container = this._ref;
    if (container) {
      container.scrollLeft = 0;
    }
  };
  componentDidMount() {
    this._resetScroll();
  }
  _onEdit = query => this.props.onEdit(query);

  render() {
    const { schema, query, makeDefaultArg } = this.props;

    if (!schema) {
      return (
        <div style={{ fontFamily: 'sans-serif' }} className="error-container">
          No Schema Available
        </div>
      );
    }
    const queryType = schema.getQueryType();
    const mutationType = schema.getMutationType();
    const subscriptionType = schema.getSubscriptionType();
    if (!queryType && !mutationType && !subscriptionType) {
      return <div>Missing query type</div>;
    }
    const queryFields = queryType && queryType.getFields();
    const mutationFields = mutationType && mutationType.getFields();
    const subscriptionFields = subscriptionType && subscriptionType.getFields();

    const parsedQuery = memoizeParseQuery(query);
    const getDefaultFieldNames =
      this.props.getDefaultFieldNames || defaultGetDefaultFieldNames;
    const getDefaultScalarArgValue =
      this.props.getDefaultScalarArgValue || defaultGetDefaultScalarArgValue;

    const definitions = parsedQuery.definitions;

    const _relevantOperations = definitions
      .map(definition => {
        if (definition.kind === 'FragmentDefinition') {
          return definition;
        } else if (definition.kind === 'OperationDefinition') {
          return definition;
        }
        return null;
      })
      .filter(Boolean);

    const relevantOperations =
      // If we don't have any relevant definitions from the parsed document,
      // then at least show an expanded Query selection
      _relevantOperations.length === 0
        ? DEFAULT_DOCUMENT.definitions
        : _relevantOperations;

    const renameOperation = (targetOperation, name) => {
      const newName =
        name == null || name === ''
          ? null
          : { kind: 'Name', value: name, loc: undefined };
      const newOperation = { ...targetOperation, name: newName };

      const existingDefs = parsedQuery.definitions;

      const newDefinitions = existingDefs.map(existingOperation => {
        if (targetOperation === existingOperation) {
          return newOperation;
        }
        return existingOperation;
      });

      return {
        ...parsedQuery,
        definitions: newDefinitions,
      };
    };

    const addOperation = kind => {
      const existingDefs = parsedQuery.definitions;

      const viewingDefaultOperation =
        parsedQuery.definitions.length === 1 &&
        parsedQuery.definitions[0] === DEFAULT_DOCUMENT.definitions[0];

      const MySiblingDefs = viewingDefaultOperation
        ? []
        : existingDefs.filter(def => {
            if (def.kind === 'OperationDefinition') {
              return def.operation === kind;
            }
            // Don't support adding fragments from explorer
            return false;
          });

      const newOperationName = `My${capitalize(kind)}${
        MySiblingDefs.length === 0 ? '' : MySiblingDefs.length + 1
      }`;

      // Add this as the default field as it guarantees a valid selectionSet
      const firstFieldName = '__typename # Placeholder value';

      const selectionSet = {
        kind: 'SelectionSet',
        selections: [
          {
            kind: 'Field',
            name: {
              kind: 'Name',
              value: firstFieldName,
              loc: null,
            },
            arguments: [],
            directives: [],
            selectionSet: null,
            loc: null,
          },
        ],
        loc: null,
      };

      const newDefinition = {
        kind: 'OperationDefinition',
        operation: kind,
        name: { kind: 'Name', value: newOperationName },
        variableDefinitions: [],
        directives: [],
        selectionSet: selectionSet,
        loc: null,
      };

      const newDefinitions =
        // If we only have our default operation in the document right now, then
        // just replace it with our new definition
        viewingDefaultOperation
          ? [newDefinition]
          : [...parsedQuery.definitions, newDefinition];

      const newOperationDef = {
        ...parsedQuery,
        definitions: newDefinitions,
      };

      this.props.onEdit(print(newOperationDef));
    };

    return (
      <div
        ref={ref => {
          this._ref = ref;
        }}
        style={{
          fontSize: 12,
          overflow: 'scroll',
          textOverflow: 'ellipsis',
          whiteSpace: 'nowrap',
          margin: 0,
          padding: 8,
          fontFamily:
            'Consolas, Inconsolata, "Droid Sans Mono", Monaco, monospace',
        }}
        className="graphiql-explorer-root"
      >
        {relevantOperations.map((operation, index) => {
          const operationName =
            operation && operation.name && operation.name.value;

          const operationKind =
            operation.kind === 'FragmentDefinition'
              ? 'fragment'
              : (operation && operation.operation) || 'query';

          const onOperationRename = newName => {
            const newOperationDef = renameOperation(operation, newName);
            this.props.onEdit(print(newOperationDef));
          };

          const fragmentType =
            operation.kind === 'FragmentDefinition' &&
            operation.typeCondition.kind === 'NamedType' &&
            schema.getType(operation.typeCondition.name.value);

          const fragmentFields =
            fragmentType instanceof GraphQLObjectType
              ? fragmentType.getFields()
              : null;

          const fields =
            operationKind === 'query'
              ? queryFields
              : operationKind === 'mutation'
              ? mutationFields
              : operationKind === 'subscription'
              ? subscriptionFields
              : operation.kind === 'FragmentDefinition'
              ? fragmentFields
              : null;

          const fragmentTypeName =
            operation.kind === 'FragmentDefinition'
              ? operation.typeCondition.name.value
              : null;

          return (
            <RootView
              key={index}
              fields={fields}
              operation={operationKind}
              name={operationName}
              definition={operation}
              onOperationRename={onOperationRename}
              onTypeName={fragmentTypeName}
              onEdit={newDefinition => {
                const newQuery = {
                  ...parsedQuery,
                  definitions: parsedQuery.definitions.map(existingDefinition =>
                    existingDefinition === operation
                      ? newDefinition
                      : existingDefinition
                  ),
                };

                const textualNewQuery = print(newQuery);

                this.props.onEdit(textualNewQuery);
              }}
              schema={schema}
              getDefaultFieldNames={getDefaultFieldNames}
              getDefaultScalarArgValue={getDefaultScalarArgValue}
              makeDefaultArg={makeDefaultArg}
              onRunOperation={() => {
                if (this.props.onRunOperation) {
                  this.props.onRunOperation(operationName);
                }
              }}
            />
          );
        })}
        <div className="variable-editor-title" style={explorerActionsStyle}>
          {queryFields ? (
            <button
              className={'toolbar-button'}
              style={buttonStyle}
              type="link"
              onClick={() => addOperation('query')}
            >
              + ADD NEW QUERY
            </button>
          ) : null}
          {mutationFields ? (
            <button
              className={'toolbar-button'}
              style={buttonStyle}
              type="link"
              onClick={() => addOperation('mutation')}
            >
              + ADD NEW MUTATION
            </button>
          ) : null}
          {subscriptionFields ? (
            <button
              className={'toolbar-button'}
              style={buttonStyle}
              type="link"
              onClick={() => addOperation('subscription')}
            >
              + ADD NEW SUBSCRIPTION
            </button>
          ) : null}
        </div>
      </div>
    );
  }
}

class ErrorBoundary extends React.Component {
  state = { hasError: false, error: null, errorInfo: null };

  componentDidCatch(error, errorInfo) {
    this.setState({ hasError: true, error: error, errorInfo: errorInfo });
    console.error('Error in component', error, errorInfo);
  }

  render() {
    if (this.state.hasError) {
      return (
        <div style={{ padding: 18, fontFamily: 'sans-serif' }}>
          <div>Something went wrong</div>
          <details style={{ whiteSpace: 'pre-wrap' }}>
            {this.state.error ? this.state.error.toString() : null}
            <br />
            {this.state.errorInfo ? this.state.errorInfo.componentStack : null}
          </details>
        </div>
      );
    }
    return this.props.children;
  }
}

class ExplorerWrapper extends React.PureComponent {
  static defaultValue = defaultValue;
  static defaultProps = {
    width: 380,
  };
  render() {
    return (
      <div
        className="historyPaneWrap"
        style={{
          height: '100%',
          width: this.props.width,
          zIndex: 7,
          display: this.props.explorerIsOpen ? 'block' : 'none',
        }}
      >
        <div className="history-title-bar">
          <div className="history-title">Explorer</div>
          <div className="doc-explorer-rhs">
            <div
              className="docExplorerHide"
              onClick={this.props.onToggleExplorer}
            >
              {'\u2715'}
            </div>
          </div>
        </div>
        <div className="history-contents">
          <ErrorBoundary>
            <Explorer {...this.props} />
          </ErrorBoundary>
        </div>
      </div>
    );
  }
}

export default ExplorerWrapper;
