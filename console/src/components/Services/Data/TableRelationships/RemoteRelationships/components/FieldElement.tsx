import React from 'react';
import { TreeFieldElement } from '../utils';
import OverlayMessage from '../../../../../Common/OverlayMessage';
import styles from '../SchemaExplorer.scss';

type Props = {
  field: TreeFieldElement;
  handleToggle: (a: TreeFieldElement) => void;
};

const FieldElement: React.FC<Props> = ({ field, handleToggle }) => {
  const toggle = () => {
    if (!field.enabled) {
      return;
    }
    handleToggle(field);
  };
  const overlayMessage = field.enabled
    ? ''
    : 'Only fields with arguments or subfields can be toggled';
  return (
    <OverlayMessage message={overlayMessage}>
      <div
        style={{
          marginLeft: `${field.depth * 20}px`,
        }}
        className={`${styles.display_flex} ${styles.add_mar_bottom_mid} ${
          field.enabled ? styles.fieldElement : styles.fieldElementDisabled
        }`}
      >
        <div
          className={`${styles.add_mar_right_small} ${styles.cursorPointer}`}
          onClick={toggle}
          role="checkbox"
          aria-checked={field.isChecked}
        >
          <input
            checked={field.isChecked}
            type="checkbox"
            className={`${styles.add_mar_right_small} ${styles.cursorPointer} legacy-input-fix`}
          />
          {field.name}
        </div>
      </div>
    </OverlayMessage>
  );
};

export default FieldElement;
