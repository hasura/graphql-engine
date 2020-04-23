import React from 'react';
import { AceEditorProps } from 'react-ace/types'
import BaseEditor from './BaseEditor';


const JSEditor = (props: AceEditorProps) => {
  return <BaseEditor {...props} mode="javascript" />;
};

export default JSEditor;
