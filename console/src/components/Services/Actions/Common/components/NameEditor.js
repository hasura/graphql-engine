import React from 'react';

import { ToolTip, Heading } from '../../../../UIKit/atoms';
import styles from './Styles.scss';

const editorLabel = 'Name';
const editorTooltip =
  'Set a name for your action. This will be a root field in your GraphQL schema';

const NameEditor = ({ value, onChange, className, placeholder }) => {
  return (
    <div className={className || ''}>
      <Heading as="h2" fontSize="15px" mt="0px" mb="xs">
        {editorLabel}
        <ToolTip message={editorTooltip} ml="sm" />
      </Heading>
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
