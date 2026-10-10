import { getTableDisplayName } from '@hasura/shared/utils';
import { useContext } from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { rootTableContext } from './RootTableProvider';
import { Card } from '@radix-ui/themes';
import { AceEditor } from '@hasura/shared/ui';

export const JsonEditor = () => {
  const { permissions, setPermissions } = useContext(rowPermissionsContext);
  const { table } = useContext(rootTableContext);

  return (
    <Card size="1" className="w-full">
      <AceEditor
        mode="json"
        onChange={(value) => {
          try {
            // Only set new permissions on valid JSON
            setPermissions(JSON.parse(value));
          } catch (error) {
            console.error(error);
          }
        }}
        minLines={1}
        fontSize={12}
        height="18px"
        width="100%"
        name={`${getTableDisplayName(table)}-json-editor`}
        value={JSON.stringify(permissions)}
        editorProps={{ $blockScrolling: true }}
        setOptions={{ useWorker: false }}
      />
    </Card>
  );
};
