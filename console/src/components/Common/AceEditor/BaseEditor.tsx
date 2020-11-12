import React from 'react';
import AceEditor, { IAceEditorProps } from 'react-ace';
import 'ace-builds/src-noconflict/ext-searchbox';
import 'ace-builds/src-noconflict/ext-language_tools';
import 'ace-builds/src-noconflict/ext-error_marker';
import 'ace-builds/src-noconflict/ext-beautify';
import 'brace/mode/html';
import 'brace/mode/markdown';
import 'brace/theme/github';
import 'brace/theme/chrome';
import 'brace/mode/sql';
import 'brace/mode/json';

import { ACE_EDITOR_THEME, ACE_EDITOR_FONT_SIZE } from './utils';

const Editor = (props: IAceEditorProps & { resizable?: boolean }) => {
  return (
    <AceEditor
      theme={ACE_EDITOR_THEME}
      fontSize={ACE_EDITOR_FONT_SIZE}
      showGutter
      tabSize={2}
      setOptions={{
        showLineNumbers: true,
      }}
      {...props}
      onLoad={
        props.resizable
          ? editorInstance => {
              editorInstance.container.style.resize = 'both';
              document.addEventListener('mouseup', () =>
                editorInstance.resize()
              );
            }
          : undefined
      }
    />
  );
};

export default Editor;
