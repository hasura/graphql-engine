import AceEditor from 'react-ace';
import { useContext } from 'react';

import { getTableDisplayName } from '../../../../../DatabaseRelationships';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { rootTableContext } from './RootTableProvider';

export const JsonEditor = () => {
  const { permissions, setPermissions } = useContext(rowPermissionsContext);
  const { table } = useContext(rootTableContext);
  return (
    <div className="p-6 rounded-lg bg-white border border-gray-200 min-h-32 w-full">
      <AceEditor
        mode="json"
        onChange={value => {
          try {
            // Only set new permissions on valid JSON
            setPermissions(JSON.parse(value));
          } catch (error) {
            console.error(error);
          }
        }}
        minLines={1}
        fontSize={14}
        height="18px"
        width="100%"
        theme="github"
        name={`${getTableDisplayName(table)}-json-editor`}
        value={JSON.stringify(permissions)}
        editorProps={{ $blockScrolling: true }}
        setOptions={{ useWorker: false }}
      />
    </div>
  );
};
