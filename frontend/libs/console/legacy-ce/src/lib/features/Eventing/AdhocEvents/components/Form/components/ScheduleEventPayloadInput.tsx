import { CodeEditorField } from '@hasura/shared/ui';
import { IAceOptions } from 'react-ace';

const editorOptions: IAceOptions = {
  fontSize: 14,
  showGutter: true,
  tabSize: 2,
  showLineNumbers: true,
  minLines: 10,
  maxLines: 10,
};

export const ScheduleEventPayloadInput = () => {
  return (
    <CodeEditorField
      name="payload"
      label="Payload"
      tooltip="The request payload for the scheduled events, should be a valid JSON"
      editorOptions={editorOptions}
      editorProps={{
        mode: 'json',
      }}
    />
  );
};
