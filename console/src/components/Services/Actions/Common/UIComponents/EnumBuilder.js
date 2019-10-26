import React from 'react';
import styles from './Styles.scss';
import { defaultEnumType, defaultEnumValue } from '../stateDefaults';
import EnumValueInput from './EnumValueInput';

const EnumBuilder = ({ type, setType }) => {
  const init = () => {
    if (type.kind !== 'enum') {
      setType({ ...defaultEnumType, name: type.name });
    }
  };

  React.useEffect(init, []);

  if (type.kind !== 'enum') return null;

  const typeNameOnChange = e => {
    setType({
      ...type,
      name: e.target.value,
    });
  };

  const setEnumValues = values => {
    setType({
      ...type,
      values: values,
    });
  };

  const enumValues = type.values;

  return (
    <div>
      <i>
        Note: Enum types are a special kind of scalar that is restricted to a
        particular set of allowed values..&nbsp;
        <a
          href="https://graphql.org/learn/schema/#enumeration-types"
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
          value={type.name}
          onChange={typeNameOnChange}
          className={`form-control ${styles.inputWidth}`}
          placeholder="__typeName"
        />
      </div>
      {enumValues.map((ev, i) => {
        const isLast = i === enumValues.length - 1;
        const setValue = value => {
          const newValues = JSON.parse(JSON.stringify(enumValues));
          newValues[i] = value;
          if (isLast && ev.value) {
            newValues.push(defaultEnumValue);
          }
          setEnumValues(newValues);
        };

        const removeValue = () => {
          setEnumValues([
            ...enumValues.slice(0, i),
            ...enumValues.slice(i + 1),
          ]);
        };

        return (
          <div className={styles.add_mar_bottom_mid}>
            <EnumValueInput
              setValue={setValue}
              removeValue={removeValue}
              isLast={isLast}
              value={ev}
            />
          </div>
        );
      })}
    </div>
  );
};

export default EnumBuilder;
