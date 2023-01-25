import React from 'react';
import AceEditor, { IAceEditorProps } from 'react-ace';
import 'ace-builds/src-noconflict/ext-searchbox';
import 'ace-builds/src-noconflict/ext-language_tools';
import 'ace-builds/src-noconflict/ext-error_marker';
import 'ace-builds/src-noconflict/ext-beautify';
import { ACE_EDITOR_THEME, ACE_EDITOR_FONT_SIZE } from './utils';

interface EditorProps extends IAceEditorProps {
  editorRef?: any;
}

const Editor: React.FC<EditorProps> = ({ mode, editorRef, ...props }) => {
  return (
    <div className="z-0 relative">
      <AceEditor
        ref={editorRef}
        mode={mode}
        theme={ACE_EDITOR_THEME}
        fontSize={ACE_EDITOR_FONT_SIZE}
        showGutter
        tabSize={2}
        setOptions={{
          showLineNumbers: true,
          useWorker: false,
        }}
        {...props}
      />
    </div>
  );
};

export default Editor;
