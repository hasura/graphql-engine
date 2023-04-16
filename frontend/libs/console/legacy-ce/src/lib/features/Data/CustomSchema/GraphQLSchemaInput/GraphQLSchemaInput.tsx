import AceEditor from 'react-ace';
import beautify from 'ace-builds/src-noconflict/ext-beautify';
import 'ace-builds/src-noconflict/mode-graphqlschema';
import { useRef } from 'react';
import { FaMagic } from 'react-icons/fa';
import { Button } from '../../../../new-components/Button';
import ReactAce from 'react-ace/lib/ace';

export type GraphQLSchemaInputProps = {
  value: string | undefined;
  onChange: (value: string) => void;
};

export const GraphQLSchemaInput: React.VFC<GraphQLSchemaInputProps> = props => {
  const { value, onChange } = props;
  const editorRef = useRef<ReactAce | null>(null);

  return (
    <div className="relative">
      <Button
        icon={<FaMagic />}
        style={{
          position: 'absolute',
          top: 10,
          right: 25,
          zIndex: 1,
        }}
        onClick={() => {
          beautify.beautify(editorRef.current?.editor.session);
        }}
      />
      <AceEditor
        ref={editorRef}
        value={value}
        onChange={onChange}
        width="100%"
        mode="graphqlschema"
        theme="eclipse"
        showGutter
        tabSize={2}
        commands={beautify.commands}
        setOptions={{
          enableBasicAutocompletion: true,
          enableLiveAutocompletion: true,
          showLineNumbers: true,
        }}
      />
    </div>
  );
};
