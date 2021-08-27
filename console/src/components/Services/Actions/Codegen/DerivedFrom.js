import React from 'react';
import styles from '../Common/components/Styles.scss';
import AceEditor from '../../../Common/AceEditor/BaseEditor';
import Tooltip from '../Common/components/Tooltip';

const DerivedFrom = ({ shouldDerive, parentMutation, toggleDerivation }) => {
  if (!parentMutation) return null;

  const tooltip =
    'This code is generated based on the assumption that operation was derived from another operation. If the assumption is wrong, you can disable the derivation.';
  return (
    <div>
      <h2 className={`${styles.subheading_text} ${styles.add_mar_bottom}`}>
        Derived operation
        <Tooltip
          id="action-name"
          text={tooltip}
          className={styles.add_mar_left_mid}
        />
      </h2>
      <div className={`${styles.add_mar_bottom}`}>
        <label className={`${styles.cursorPointer}`} onClick={toggleDerivation}>
          <input
            type="checkbox"
            checked={shouldDerive}
            className={`${styles.cursorPointer} ${styles.add_mar_right_mid} legacy-input-fix`}
          />
          Generate code with delegation to the derived mutation
        </label>
      </div>
      <AceEditor
        mode="graphql"
        value={parentMutation}
        width={'600px'}
        height={'200px'}
        readOnly
      />
    </div>
  );
};

export default DerivedFrom;
