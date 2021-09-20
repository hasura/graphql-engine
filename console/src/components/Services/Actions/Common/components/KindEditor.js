import React from 'react';
import styles from './Styles.scss';
import KnowMoreLink from '../../../../Common/KnowMoreLink/KnowMoreLink';
import ToolTip from '../../../../Common/Tooltip/Tooltip';

const editorLabel = 'Kind';
const docsRef =
  'https://docs.hasura.io/1.0/graphql/manual/actions/async-actions.html';

const KindEditor = ({ value, onChange, className, disabled = false }) => {
  const setAsynchronous = () => {
    if (!disabled) onChange('asynchronous');
  };

  const setSynchronous = () => {
    if (!disabled) onChange('synchronous');
  };

  return (
    <div className={className || ''}>
      <h2
        className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
      >
        {editorLabel}
        <ToolTip
          message={'Toggle between synchronous vs asynchronous action types'}
        />
        <KnowMoreLink href={docsRef} />
      </h2>
      <div className={styles.display_flex}>
        <label
          className={`${styles.add_mar_right} ${styles.cursorPointer}`}
          onClick={setSynchronous}
        >
          <input
            type="radio"
            checked={value === 'synchronous'}
            readOnly
            className={`${styles.add_mar_right_small} legacy-input-fix`}
            disabled={disabled}
          />
          Synchronous
        </label>
        <label
          onClick={setAsynchronous}
          className={`${styles.add_mar_right} ${styles.cursorPointer}`}
        >
          <input
            type="radio"
            readOnly
            checked={value === 'asynchronous'}
            className={`legacy-input-fix ${styles.add_mar_right_small}`}
            disabled={disabled}
          />
          Asynchronous
        </label>
      </div>
    </div>
  );
};

export default KindEditor;
