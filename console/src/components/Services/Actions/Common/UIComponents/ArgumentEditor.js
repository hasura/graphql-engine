import React from 'react';
import styles from './Styles.scss';
import ArgumentInput from './ArgumentInput';

const ArgumentEditor = ({ argument, allTypes, setArgument }) => {
  return (
    <div className={styles.add_mar_bottom_mid}>
      <ArgumentInput
        argument={argument}
        setArgument={setArgument}
        allTypes={allTypes}
      />
    </div>
  );
};

export default ArgumentEditor;
