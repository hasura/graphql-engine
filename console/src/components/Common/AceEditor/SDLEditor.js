import React from 'react';
import AceEditor from 'react-ace';
import { ACE_EDITOR_THEME, ACE_EDITOR_FONT_SIZE } from './utils';
import 'ace-builds/src-noconflict/ext-searchbox';
import 'ace-builds/src-noconflict/ext-language_tools';
import 'ace-builds/src-noconflict/ext-error_marker';
import 'ace-builds/src-noconflict/ext-beautify';

const Editor = ({ value, onChange, ...props }) => {
  return (
    <AceEditor
      placeholder="## Action definition as a mutation in SDL"
      mode="graphqlschema"
      theme={ACE_EDITOR_THEME}
      onChange={onChange}
      fontSize={ACE_EDITOR_FONT_SIZE}
      showPrintMargine
      showGutter
      tabSize={2}
      setOptions={{
        showLineNumbers: false,
        enableBasicAutocompletion: true,
        enableSnippets: true,
        behavioursEnabled: true,
      }}
      value={value}
      {...props}
    />
  );
};

export default Editor;
