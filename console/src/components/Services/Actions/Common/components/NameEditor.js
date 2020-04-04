import React from 'react';

import { ToolTip } from '../../../../UIKit/atoms';
import styles from './Styles.scss';

const editorLabel = 'Name';
const editorTooltipText =
  'Set a name for your action. This will be a root field in your GraphQL schema';

const NameEditor = ({ value, onChange, className, placeholder }) => {
  return (
    <div className={className || ''}>
      <h2
        className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
      >
        {editorLabel}
        <ToolTip message={editorTooltipText} ml="sm" />
      </h2>
      <input
        type="text"
        value={value}
        onChange={onChange}
        placeholder={placeholder}
        className={`form-control ${styles.inputWidth}`}
      />
    </div>
  );
};

export default NameEditor;
