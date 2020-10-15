import React from 'react';
import AceEditor from 'react-ace';
import 'brace/mode/markdown';
import 'brace/theme/github';

type ResizableAceEditorProps = {
  id: string;
  allProps: any;
  mode: string;
  theme: string;
  name: string;
  minLines: number;
  maxLines: number;
  value: any;
  showPrintMargin: boolean;
  onChange: any;
  showGutter: boolean;
  commands: any;
};

const ResizableAceEditor = (props: ResizableAceEditorProps) => {
  return (
    <AceEditor
      key={props.id}
      {...props.allProps}
      mode={props.mode}
      theme={props.theme}
      name={props.name}
      minLines={props.minLines}
      maxLines={props.maxLines}
      value={props.value}
      showPrintMargin={props.showPrintMargin}
      onChange={props.onChange}
      showGutter={props.showGutter}
      focus
      width="100%"
      commands={props.commands}
      onLoad={editorInstance => {
        editorInstance.container.style.resize = 'both';
        document.addEventListener('mouseup', () => editorInstance.resize());
      }}
    />
  );
};

export default ResizableAceEditor;
