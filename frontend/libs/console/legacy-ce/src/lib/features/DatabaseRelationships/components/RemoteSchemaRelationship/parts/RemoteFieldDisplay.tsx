import { buildServerRemoteFieldObject } from './RemoteSchemaTree';
import { RelationshipFields } from '../types';

export const RemoteFieldDisplay = ({
  relationshipFields,
}: {
  relationshipFields: RelationshipFields[];
}) => {
  return (
    <input
      className="pl-sm mt-xs block h-input w-full shadow-sm rounded cursor-not-allowed bg-gray-100 border border-gray-300"
      disabled
      value={JSON.stringify(buildServerRemoteFieldObject(relationshipFields))}
    />
  );
};
