import React from 'react';
import { AceEditorProps } from 'react-ace/types'
import BaseEditor from './BaseEditor';


const SDLEditor = (props: AceEditorProps) => {
  return <BaseEditor mode="graphqlschema" {...props} />;
};

export default SDLEditor;
