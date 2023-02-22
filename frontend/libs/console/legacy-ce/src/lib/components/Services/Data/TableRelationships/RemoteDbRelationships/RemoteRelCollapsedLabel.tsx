import { RemoteDBRelationship } from '../../../../../metadata/types';
import React from 'react';
import { parseDbToDbRemoteRel } from './utils';

type Props = {
  currentSource: string;
  currentSchema: string;
  currentTable: string;
  relationship?: RemoteDBRelationship;
};

export const RemoteRelCollapsedLabel: React.FC<Props> = ({
  currentSource,
  currentSchema,
  currentTable,
  relationship,
}) => {
  if (!relationship) {
    return null;
  }
  const parseRelationship = parseDbToDbRemoteRel(relationship);

  return (
    <div className="flex">
      <div>
        <b>{`${parseRelationship.relName}`}</b>&nbsp;
      </div>
      <div>
        <i>
          {`- ${currentSource}.${currentSchema}.${currentTable} → ${parseRelationship.relSource}.${parseRelationship.relTable.name}
          `}
        </i>
      </div>
    </div>
  );
};
