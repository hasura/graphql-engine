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

export const CronPayloadInput = () => {
  return (
    <CodeEditorField
      name="payload"
      label="Payload"
      tooltip="The request payload for the cron trigger, should be a valid JSON"
      editorOptions={editorOptions}
      editorProps={{
        mode: 'json',
      }}
    />
  );
};
