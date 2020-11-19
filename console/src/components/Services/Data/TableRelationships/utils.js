import React from 'react';
import {
  isInputObjectType,
  isInterfaceType,
  isEnumType,
  isObjectType,
  isScalarType,
  isWrappingType,
} from 'graphql';

/* This function sets the styling to the way the relationship looks, for eg: article.id -> user.user_id */
export const getRelDef = relMeta => {
  const lcol =
    relMeta.lcol.length > 1
      ? '( ' + relMeta.lcol.join(', ') + ' )'
      : relMeta.lcol[0];
  const rcol =
    relMeta.rcol.length > 1
      ? '( ' + relMeta.rcol.join(', ') + ' )'
      : relMeta.rcol[0];

  return relMeta.isObjRel ? (
    <>
      {relMeta.lTable} . {lcol} &nbsp;&rarr;&nbsp;
      {relMeta.rTable} . {rcol}
    </>
  ) : (
    <>
      {relMeta.rTable} . {rcol} &nbsp;&rarr;&nbsp;
      {relMeta.lTable} . {lcol}
    </>
  );
};

/* Gets the complete list of relationships and converts it to a list of object, which looks like so :
[ { objRel: {objectRelationship}, arrRel: {arrayRelationship} } ] */
export const getObjArrRelList = relationships => {
  const objRels = relationships.filter(r => r.rel_type === 'object');
  const arrRels = relationships.filter(r => r.rel_type !== 'object');
  const requiredList = [];
  const length =
    objRels.length > arrRels.length ? objRels.length : arrRels.length;
  for (let i = 0; i < length; i++) {
    const objRel = objRels[i] ? objRels[i] : null;
    const arrRel = arrRels[i] ? arrRels[i] : null;

    requiredList.push({
      objRel,
      arrRel,
    });
  }
  return requiredList;
};

const getUnderlyingType = t => {
  let _type = t;
  while (isWrappingType(_type)) {
    _type = _type.ofType;
  }
  return _type;
};

export const getSchemaTree = (relationship, fields) => {
  const { remoteField } = relationship;
  const schemaTree = [];

  const isArgChecked = (
    arg,
    fieldNesting,
    argNesting,
    parentField,
    parentArg
  ) => {
    if (parentField.arguments) {
      const search = parentField.arguments.find(
        a =>
          a.name === arg.name &&
          a.argNesting === argNesting &&
          a.parentArg === parentArg
      );
      if (search) {
        return {
          column: search.column,
          static: search.static,
        };
      }
    }
    return false;
  };

  const handleArg = (arg, nesting, argNesting, parentField, parentArg) => {
    const isChecked = isArgChecked(
      arg,
      nesting,
      argNesting,
      parentField,
      parentArg
    );
    schemaTree.push({
      name: arg.name,
      type: getUnderlyingType(arg.type).name,
      nesting,
      argNesting,
      isChecked: !!isChecked,
      column: isChecked ? isChecked.column : false,
      static: isChecked ? isChecked.static : false,
      isScalar:
        isScalarType(getUnderlyingType(arg.type)) ||
        isEnumType(getUnderlyingType(arg.type)),
      isArg: true,
      parentFieldName: parentField.name,
      parentFieldNesting: parentField.nesting,
      parentArg,
    });
    if (isChecked) {
      const handleWrappingTypeArg = __fieldtype => {
        const currentFieldType = getUnderlyingType(__fieldtype);
        if (currentFieldType._fields) {
          Object.values(currentFieldType._fields).forEach(fa =>
            handleArg(
              fa,
              nesting,
              argNesting + 1,
              parentField,
              `${parentArg}.${arg.name}`
            )
          );
        }
      };
      const handleInputObjectTypeArg = __fieldtype => {
        if (__fieldtype._fields) {
          Object.values(__fieldtype._fields).forEach(fa =>
            handleArg(
              fa,
              nesting,
              argNesting + 1,
              parentField,
              `${parentArg}.${arg.name}`
            )
          );
        }
      };
      if (isWrappingType(arg.type)) {
        handleWrappingTypeArg(arg.type);
      } else if (isInputObjectType(arg.type) || isInterfaceType(arg.type)) {
        handleInputObjectTypeArg(arg.type);
      }
    }
  };

  const isFieldChecked = (field, nesting) => {
    if (
      remoteField.find(rf => field.name === rf.name && nesting === rf.nesting)
    ) {
      return true;
    }
    return false;
  };
  const handleField = (field, nesting) => {
    if (isScalarType(getUnderlyingType(field.type))) {
      if (!field.args || (field.args && field.args.length === 0)) {
        return;
      }
    }
    const isChecked = isFieldChecked(field, nesting);
    schemaTree.push({
      name: field.name,
      nesting,
      type: field.type,
      isChecked,
    });
    if (isChecked) {
      const currentSelectedField = remoteField.find(
        rf => field.name === rf.name && nesting === rf.nesting
      );
      field.args
        .sort((fa1, fa2) => {
          return fa1.name > fa2.name ? 1 : fa1.name < fa2.name ? -1 : 0;
        })
        .forEach(fa => {
          handleArg(fa, nesting + 1, 0, currentSelectedField, '');
        });

      const handleScalarTypeField = () => {};

      const handleObjectTypeField = __fieldtype => {
        Object.values(__fieldtype._fields).forEach(f =>
          handleField(f, nesting + 1)
        );
      };

      const handleListTypeField = __fieldtype => {
        const unwrappedType = getUnderlyingType(__fieldtype);
        if (isObjectType(unwrappedType) || isInterfaceType(unwrappedType)) {
          handleObjectTypeField(unwrappedType);
        } else {
          handleScalarTypeField(unwrappedType);
        }
      };

      if (isWrappingType(field.type) || isWrappingType(field.type)) {
        handleListTypeField(field.type);
      } else if (isObjectType(field.type) || isInterfaceType(field.type)) {
        handleObjectTypeField(field.type);
      } else {
        handleScalarTypeField(field.type);
      }
    }
  };

  fields.forEach(f => handleField(f, 0));

  return schemaTree;
};

const getTypedInput = (input_, argType, argName) => {
  const throwError = () => {
    throw Error(
      `Invalid static input for argument "${argName}" of type "${argType}".`
    );
  };
  const input = input_.trim();
  if (argType === 'Int') {
    const intVal = parseInt(input, 10);
    if (isNaN(intVal)) {
      throwError();
    }
    return intVal;
  }
  if (argType === 'Boolean') {
    if (input.toLowerCase() === 'true') return true;
    if (input.toLowerCase() === 'false') return false;
    throwError();
  }
  if (argType === 'Float') {
    const floatVal = parseFloat(input);
    if (isNaN(floatVal)) {
      throwError();
    }
    return floatVal;
  }
  return input;
};

export const getRemoteRelPayload = (remoteRel, table) => {
  const payload = {};
  const { remoteField, name, remoteSchema } = remoteRel;
  payload.name = name;
  payload.remote_schema = remoteSchema;
  payload.table = table;
  const hasuraFields = [];
  const getArgs = (field, argNesting, parentArg, _argObj) => {
    const argObj = { ..._argObj };
    field.arguments.forEach(a => {
      if (a.argNesting === argNesting && parentArg === a.parentArg) {
        if (a.column) {
          argObj[a.name] = `$${a.column}`;
          hasuraFields.push(a.column);
        } else if (a.static) {
          argObj[a.name] = getTypedInput(a.static, a.type, a.name);
        } else {
          argObj[a.name] = getArgs(
            field,
            argNesting + 1,
            `${parentArg}.${a.name}`,
            {}
          );
        }
      }
    });
    return argObj;
  };

  const getRemoteFieldObj = nesting => {
    const _rf = remoteField.find(rf => rf.nesting === nesting);
    if (!_rf) {
      return undefined;
    }
    const _field = {
      [_rf.name]: {
        arguments: getArgs(_rf, 0, '', {}),
        field: getRemoteFieldObj(nesting + 1),
      },
    };
    if (_field[_rf.name].field === undefined) {
      delete _field[_rf.name].field;
    }
    return _field;
  };

  payload.remote_field = getRemoteFieldObj(0);
  payload.hasura_fields = [];
  hasuraFields.forEach(hf => {
    if (hf.constructor.name === 'Array') {
      hf.forEach(f => payload.hasura_fields.push(f));
    } else {
      payload.hasura_fields.push(hf);
    }
  });
  payload.hasura_fields = [...new Set(payload.hasura_fields)];

  return payload;
};

export const parseRemoteRelationship = remoteRel => {
  let _remoteField = { ...remoteRel.remote_field };
  const remoteFields = [];
  let nesting = 0;

  const getArgs = field => {
    const argsList = [];
    const serialiseArgs = (args, argNesting, parentArg) => {
      Object.keys(args).forEach((a, i) => {
        const argObj = {
          name: a,
          parentArg,
          argNesting,
        };
        const argValue = Object.values(args)[i];
        if (typeof argValue === 'string') {
          if (argValue[0] === '$') {
            argObj.column = argValue.substr(1);
          } else {
            argObj.static = argValue;
            argObj.type = 'String';
          }
        } else if (typeof argValue === 'number') {
          argObj.static = argValue.toString();
          argObj.type = 'Int';
        } else if (typeof argValue === 'boolean') {
          argObj.static = argValue.toString();
          argObj.type = 'Boolean';
        }
        argsList.push(argObj);
        if (typeof argValue === 'object') {
          serialiseArgs(argValue, argNesting + 1, `${parentArg}.${a}`);
        }
      });
    };
    serialiseArgs(field.arguments, 0, '');
    return argsList;
  };

  while (_remoteField && Object.keys(_remoteField).length > 0) {
    remoteFields.push({
      name: Object.keys(_remoteField)[0],
      nesting,
      arguments: getArgs(Object.values(_remoteField)[0]),
    });
    _remoteField = Object.values(_remoteField)[0].field;
    nesting++;
  }
  return {
    name: remoteRel.name,
    remoteSchema: remoteRel.remote_schema,
    remoteField: remoteFields,
  };
};

export const getRemoteRelConfig = (rel, tableName, styles) => {
  if (!rel.remoteSchema) {
    return '';
  }
  const remoteField = rel.remoteField.find(f => f.nesting === 0);
  if (!remoteField) return '';

  return (
    <div className={styles.display_flex}>
      <div>
        <b>{`${rel.name}`}</b>&nbsp;
      </div>
      <div>
        <i>{`- ${tableName} → ${rel.remoteSchema} . ${remoteField.name}`}</i>
      </div>
    </div>
  );
};
