import React from 'react';
import styles from './Styles.scss';
import { defaultScalarType } from '../stateDefaults';

const ScalarBuilder = ({ type, setType }) => {
  const init = () => {
    if (type.kind !== 'scalar') {
      setType({ ...defaultScalarType, name: type.name });
    }
  };

  React.useEffect(init, []);

  const typeNameOnChange = e => {
    setType({
      ...type,
      name: e.target.value,
    });
  };

  return (
    <div>
      <i>
        Note: Scalars accept plain values.&nbsp;
        <a
          href="https://graphql.org/learn/schema/#scalar-types"
          rel="noopener noreferrer"
          target="_blank"
        >
          (Read more)
        </a>
      </i>
      <div className={`${styles.add_mar_top}`}>
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
    </div>
  );
};

export default ScalarBuilder;
