import React from 'react';
import AceEditor from 'react-ace';
import { ACE_EDITOR_THEME, ACE_EDITOR_FONT_SIZE } from './utils';
import 'ace-builds/src-noconflict/ext-searchbox';
import 'ace-builds/src-noconflict/ext-language_tools';
import 'ace-builds/src-noconflict/ext-error_marker';
import 'ace-builds/src-noconflict/ext-beautify';

const Editor = ({ mode, ...props }) => {
  return (
    <AceEditor
      mode={mode}
      theme={ACE_EDITOR_THEME}
      fontSize={ACE_EDITOR_FONT_SIZE}
      showPrintMargine
      showGutter
      tabSize={2}
      setOptions={{
        showLineNumbers: true,
        enableBasicAutocompletion: true,
        enableSnippets: true,
        behavioursEnabled: true,
      }}
      {...props}
    />
  );
};

export default Editor;
