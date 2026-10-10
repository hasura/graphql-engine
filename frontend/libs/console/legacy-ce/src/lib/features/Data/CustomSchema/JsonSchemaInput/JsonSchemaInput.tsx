import beautify from 'ace-builds/src-noconflict/ext-beautify';
import { useRef } from 'react';
import { FaMagic } from 'react-icons/fa';
import { AceEditor, AceEditorRef, IconButton } from '@hasura/shared/ui';

export type JsonSchemaInputProps = {
  value: string | undefined;
  onChange: (value: string) => void;
};

export const JsonSchemaInput: React.FC<JsonSchemaInputProps> = (props) => {
  const { value, onChange } = props;
  const editorRef = useRef<AceEditorRef | null>(null);

  return (
    <div className="relative">
      <IconButton
        mode="default"
        style={{
          position: 'absolute',
          top: 10,
          right: 20,
          zIndex: 1,
        }}
        onClick={() => {
          if (editorRef.current?.editor.session) {
            beautify.beautify(editorRef.current?.editor.session);
          }
        }}
      >
        <FaMagic />
      </IconButton>
      <AceEditor
        ref={editorRef}
        value={value}
        onChange={onChange}
        width="100%"
        height="300px"
        mode="json"
        showGutter
        tabSize={2}
        setOptions={{
          enableBasicAutocompletion: true,
          enableLiveAutocompletion: true,
          showLineNumbers: true,
        }}
      />
    </div>
  );
};
