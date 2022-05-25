import React from 'react';
import { inputStyles } from '../../constants';
import Tooltip from './Tooltip';

const editorLabel = 'Name';
const editorTooltip =
  'Set a name for your action. This will be a root field in your GraphQL schema';

const NameEditor = ({ value, onChange, className, placeholder }) => {
  return (
    <div className={className || ''}>
      <h2 className="text-sm font-bold pb-5 mb-1.5">
        {editorLabel}
        <Tooltip id="action-name" text={editorTooltip} className="ml-2.5" />
      </h2>
      <input
        type="text"
        value={value}
        onChange={onChange}
        placeholder={placeholder}
        className={`${inputStyles} w-52`}
      />
    </div>
  );
};

export default NameEditor;
