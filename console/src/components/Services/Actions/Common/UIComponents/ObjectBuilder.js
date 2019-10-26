import React from 'react';
import styles from './Styles.scss';
import ArgumentEditor from './ArgumentEditor';
import FieldEditor from './FieldEditor';
import { defaultObjectType, defaultArg, defaultField } from '../stateDefaults';

const ObjectBuilder = ({ type, setType, argTypes, fieldTypes }) => {
  const { name, arguments: args, fields, kind } = type;

  const init = () => {
    if (kind !== 'object') {
      setType({
        ...defaultObjectType,
        name,
      });
    }
  };

  React.useEffect(init, []);

  const typeNameOnChange = e => {
    setType({
      ...type,
      name: e.target.value,
    });
  };

  const setArguments = a => {
    const newArgs = [...a];
    const lastArg = newArgs[newArgs.length - 1];
    if (lastArg.name && lastArg.type) {
      newArgs.push({ ...defaultArg });
    }
    setType({
      ...type,
      arguments: newArgs,
    });
  };

  const setFields = f => {
    const newFields = [...f];
    const lastField = newFields[newFields.length - 1];
    if (lastField.name && lastField.type) {
      newFields.push({ ...defaultField });
    }
    setType({
      ...type,
      fields: newFields,
    });
  };

  if (kind !== 'object') return null;

  return (
    <div>
      <i>
        Note: The most basic components of a GraphQL schema are object types,
        which just represent a kind of object you can fetch from your service,
        and what fields it has.&nbsp;
        <a
          href="https://graphql.org/learn/schema/#object-types-and-fields"
          rel="noopener noreferrer"
          target="_blank"
        >
          (Read more)
        </a>
      </i>
      <div className={`${styles.add_mar_top} ${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom}`}>
          <b>Type name:</b>
        </div>
        <input
          type="text"
          value={name}
          onChange={typeNameOnChange}
          className={`form-control ${styles.inputWidth}`}
          placeholder="__typeName"
        />
      </div>
      <div>
        <div className={`${styles.add_mar_bottom}`}>
          <b>Fields:</b>
        </div>
        {fields.map((f, i) => {
          const isLast = i === fields.length - 1;
          const setField = field => {
            const newFields = JSON.parse(JSON.stringify(fields));
            newFields[i] = field;
            setFields(newFields);
          };
          const removeField = () => {
            const newFields = JSON.parse(JSON.stringify(fields));
            setFields([...newFields.slice(0, i), ...newFields.slice(i + 1)]);
          };
          return (
            <FieldEditor
              field={f}
              setField={setField}
              removeField={removeField}
              allTypes={fieldTypes}
              isLast={isLast}
              index={i}
            />
          );
        })}
      </div>
    </div>
  );
};

export default ObjectBuilder;
