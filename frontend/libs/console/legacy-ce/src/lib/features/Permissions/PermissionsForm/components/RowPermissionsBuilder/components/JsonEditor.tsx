import { getTableDisplayName } from '@hasura/shared/utils';
import { useContext, useState } from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { rootTableContext } from './RootTableProvider';
import { Card } from '@radix-ui/themes';
import { AceEditor } from '@hasura/shared/ui';

const formatJson = (value: unknown) => JSON.stringify(value, null, 2);

type Annotation = { row: number; column: number; type: string; text: string };

/** Points the parse error at its line, from the position the error reports. */
export const getJsonErrorAnnotation = (
  text: string,
  error: unknown,
): Annotation => {
  const message = (error instanceof Error ? error.message : String(error))
    // V8 quotes the whole input in some messages
    .replace(/, ".*" is not valid JSON$/s, '');
  const lineColumn = message.match(/line (\d+) column (\d+)/);
  const position = message.match(/position (\d+)/);

  const pointAt = (offset: number) => {
    const lines = text.slice(0, offset).split('\n');
    return { row: lines.length - 1, column: lines[lines.length - 1].length };
  };

  // without a position (e.g. "Unexpected token"), the first line is marked
  let location = { row: 0, column: 0 };
  if (lineColumn) {
    location = {
      row: Number(lineColumn[1]) - 1,
      column: Number(lineColumn[2]) - 1,
    };
  } else if (position) {
    location = pointAt(Number(position[1]));
  } else if (/end of JSON input/.test(message)) {
    location = pointAt(text.length);
  }

  return { ...location, type: 'error', text: message };
};

export const JsonEditor = () => {
  const { permissions, setPermissions } = useContext(rowPermissionsContext);
  const { table } = useContext(rootTableContext);

  // the text is kept while it's invalid JSON, and only valid JSON is applied
  const [text, setText] = useState(() => formatJson(permissions));
  const [annotations, setAnnotations] = useState<Annotation[]>([]);

  // reformat when the permissions change outside of the editor
  const serializedPermissions = JSON.stringify(permissions);
  const [syncedPermissions, setSyncedPermissions] = useState(
    serializedPermissions,
  );
  if (serializedPermissions !== syncedPermissions) {
    setSyncedPermissions(serializedPermissions);
    setText(formatJson(permissions));
    setAnnotations([]);
  }

  return (
    <Card size="1" className="w-full">
      <AceEditor
        mode="json"
        onChange={(value) => {
          setText(value);
          try {
            const parsed = JSON.parse(value);
            setAnnotations([]);
            setSyncedPermissions(JSON.stringify(parsed));
            setPermissions(parsed);
          } catch (error) {
            setAnnotations([getJsonErrorAnnotation(value, error)]);
          }
        }}
        minLines={8}
        maxLines={30}
        fontSize={12}
        width="100%"
        name={`${getTableDisplayName(table)}-json-editor`}
        value={text}
        annotations={annotations}
        editorProps={{ $blockScrolling: true }}
        setOptions={{ useWorker: false }}
      />
    </Card>
  );
};
