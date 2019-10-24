import React from 'react';
import styles from './Styles.scss';
import FieldEditor from './FieldEditor';
import { defaultInputObjectType, defaultField } from '../stateDefaults';

const InputObjectBuilder = ({ type, setType, fieldTypes }) => {
  const { name, fields, kind } = type;

  const init = () => {
    if (type.kind !== 'input_object') {
      setType({
        ...defaultInputObjectType,
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

  if (kind !== 'input_object') return null;

  return (
    <div>
      <i>
        Note: An input object defines a structured collection of fields which
        may be supplied to a field argument.&nbsp;
        <a
          href="https://graphql.org/learn/schema/#input-types"
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
            setFields([...newFields.slice(0, i), newFields.slice(i + 1)]);
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

export default InputObjectBuilder;
