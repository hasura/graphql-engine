import React from 'react';

import { Driver } from '@/dataSources';
import { DataTarget } from '@/features/Datasources';
import { DbToRsForm } from '@/features/RemoteRelationships';
import { RowData } from '@/features/RelationshipsTable';
import { DbToRemoteSchemaRelationship } from '@/features/MetadataAPI';

import { RemoteDBRelationshipWidget } from '../RemoteDBRelationshipWidget';
import { LocalRelationshipWidget } from '../LocalDBRelationshipWidget';
import { RelOption } from './utils';

const getExistingRelationshipType = (
  existingRelationship: RowData
): RelOption => {
  if (existingRelationship.toType === 'database') return 'remoteDatabase';
  if (existingRelationship.toType === 'remote_schema') return 'remoteSchema';

  return 'local';
};

type EditRelationshipFormProps = {
  driver: Driver;
  sourceTableInfo: DataTarget;
  existingRelationship: RowData;
  /**
   * optional callback function, can be used to get the onComplete event, this could be a onSuccess, or onError event.
   *
   */
  onComplete: (callback: {
    title?: string;
    message?: string;
    type: 'success' | 'error' | 'cancel';
  }) => void;
};

export const EditRelationshipForm = ({
  sourceTableInfo,
  driver,
  existingRelationship,
  onComplete,
}: EditRelationshipFormProps) => {
  const option = React.useMemo(
    () => getExistingRelationshipType(existingRelationship),
    [existingRelationship]
  );

  switch (option) {
    case 'local':
      return (
        <LocalRelationshipWidget
          key={existingRelationship.name}
          driver={driver}
          sourceTableInfo={sourceTableInfo}
          existingRelationshipName={existingRelationship.name}
          onComplete={onComplete}
        />
      );
    case 'remoteDatabase':
      return (
        <RemoteDBRelationshipWidget
          key={existingRelationship.name}
          sourceTableInfo={sourceTableInfo}
          existingRelationshipName={existingRelationship.name}
          onComplete={onComplete}
        />
      );
    case 'remoteSchema':
      // asserting type because here RS relationship is the only possibility, DB relationships are filtered out above with the help of `option`
      // existing relationship would handle the legacy/ new shape,
      // ie. lhs_fields will always be present (legacy hasura_field is already translated to the new format)
      const { lhs_fields, remote_field, remoteSchemaName } =
        existingRelationship.relationship as DbToRemoteSchemaRelationship;

      return (
        <DbToRsForm
          key={existingRelationship.name}
          sourceTableInfo={sourceTableInfo}
          onComplete={onComplete}
          selectedRelationship={{
            name: existingRelationship.name,
            definition: {
              to_remote_schema: {
                lhs_fields,
                remote_field,
                remote_schema: remoteSchemaName,
              },
            },
          }}
        />
      );
    default:
      // This is a TS protection that forces the developer to properly manage all the cases.
      // It throws when the developer adds new values to RelOption without adding a corresponding `case` here.
      throw new Error(`Unknown RelOption: ${option}`);
  }
};
