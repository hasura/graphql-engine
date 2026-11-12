import { getTableDisplayName } from '@hasura/shared/utils';
import { useContext, useState } from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { rootTableContext } from './RootTableProvider';
import { Card } from '@radix-ui/themes';
import { JSONEditor } from '@hasura/shared/ui';

const formatJson = (value: unknown) => JSON.stringify(value, null, 2);

export const JsonEditor = () => {
  const { permissions, setPermissions } = useContext(rowPermissionsContext);
  const { table } = useContext(rootTableContext);

  // the text is kept while it's invalid JSON, and only valid JSON is applied
  const [text, setText] = useState(() => formatJson(permissions));

  // reformat when the permissions change outside of the editor
  const serializedPermissions = JSON.stringify(permissions);
  const [syncedPermissions, setSyncedPermissions] = useState(
    serializedPermissions,
  );
  if (serializedPermissions !== syncedPermissions) {
    setSyncedPermissions(serializedPermissions);
    setText(formatJson(permissions));
  }

  return (
    <Card size="1" className="w-full">
      <JSONEditor
        data={text}
        onChange={(value) => {
          setText(value);
          try {
            const parsed = JSON.parse(value);
            setSyncedPermissions(JSON.stringify(parsed));
            setPermissions(parsed);
          } catch {
            // the editor marks the parse error
          }
        }}
        minLines={8}
        maxLines={30}
        fontSize={12}
        name={`${getTableDisplayName(table)}-json-editor`}
        editorProps={{ $blockScrolling: true }}
      />
    </Card>
  );
};
