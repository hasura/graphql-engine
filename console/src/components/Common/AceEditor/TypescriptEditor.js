import React from 'react';
import BaseEditor from './BaseEditor';

const Editor = ({ value, onChange, ...props }) => {
  return <BaseEditor {...props} mode="typescript" />;
};

export default Editor;
