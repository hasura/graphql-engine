import React from 'react';
import { AceEditorProps } from 'react-ace/types'
import BaseEditor from './BaseEditor';


const Editor = ({ value, onChange, ...props }: AceEditorProps) => {
  return <BaseEditor {...props} mode="typescript" />;
};

export default Editor;
