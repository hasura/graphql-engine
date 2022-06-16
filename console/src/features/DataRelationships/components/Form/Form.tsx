import { Button } from '@/new-components/Button';
import React, { useEffect, useState } from 'react';
import { CardRadioGroup } from '@/new-components/CardRadioGroup';
import { DbToRsForm } from '@/features/RemoteRelationships';
import { RowData } from '@/features/RelationshipsTable';
import { DataTarget } from '@/features/Datasources';
import { DbToRemoteSchemaRelationship } from '@/features/MetadataAPI';
import { Driver } from '@/dataSources';
import { LocalRelationshipWidget } from '../LocalDBRelationshipWidget';
import { RemoteDBRelationshipWidget } from '../RemoteDBRelationshipWidget';

type RelOption =
  | 'Local Relationship'
  | 'Remote Database Relationship'
  | 'Remote Schema Relationship';

const data: { value: RelOption; title: string; body: string }[] = [
  {
    value: 'Local Relationship',
    title: 'Local Relationship',
    body: 'Relationships from this table to a local database table.',
  },
  {
    value: 'Remote Database Relationship',
    title: 'Remote Database Relationship',
    body: 'Relationship from this local table to a remote database table.',
  },
  {
    value: 'Remote Schema Relationship',
    title: 'Remote Schema Relationship',
    body: 'Relationship from this local table to a remote schema.',
  },
];

interface FormProps {
  existingRelationship?: RowData;
  sourceTableInfo: DataTarget;
  driver: Driver;
  onComplete: (v: {
    title?: string;
    message?: string;
    type: 'success' | 'error' | 'cancel';
  }) => void;
}

const getExistingRelationshipType = (
  existingRelationship?: RowData
): RelOption => {
  if (!existingRelationship) return 'Local Relationship';
  if (existingRelationship.toType === 'database')
    return 'Remote Database Relationship';
  if (existingRelationship.toType === 'remote_schema')
    return 'Remote Schema Relationship';

  return 'Local Relationship';
};

export const Form = ({
  existingRelationship,
  sourceTableInfo,
  onComplete,
  driver,
}: FormProps) => {
  const [option, setOption] = useState<RelOption>(
    getExistingRelationshipType(existingRelationship)
  );

  useEffect(() => {
    setOption(getExistingRelationshipType(existingRelationship));
  }, [existingRelationship]);

  return (
    <div className="w-full sm:w-9/12 bg-white shadow-sm rounded p-md border border-gray-300 show">
      <div className="flex items-center mb-md">
        <Button size="sm" onClick={() => onComplete({ type: 'cancel' })}>
          Cancel
        </Button>
        <span className="font-semibold text-muted ml-1.5">
          Create New Relationship
        </span>
      </div>
      <hr className="mb-md border-gray-300" />
      <div className="mb-md">
        <p className="mb-sm text-muted font-semibold">
          Select a Relationship Method
        </p>
        <CardRadioGroup
          items={data}
          onChange={relType => setOption(relType)}
          value={option}
          disabled={!!existingRelationship}
        />
        {option === 'Local Relationship' ? (
          <LocalRelationshipWidget
            sourceTableInfo={sourceTableInfo}
            existingRelationshipName={existingRelationship?.name}
            onComplete={onComplete}
            driver={driver}
          />
        ) : option === 'Remote Database Relationship' ? (
          <RemoteDBRelationshipWidget
            onComplete={onComplete}
            sourceTableInfo={sourceTableInfo}
            existingRelationshipName={existingRelationship?.name}
          />
        ) : (
          <DbToRsForm
            sourceTableInfo={sourceTableInfo}
            onComplete={onComplete}
            selectedRelationship={
              existingRelationship && {
                name: existingRelationship.name,
                definition: {
                  to_remote_schema: {
                    lhs_fields: (existingRelationship.relationship as DbToRemoteSchemaRelationship) // asserting type becase here RS relationship is the only possibility, DB relationships are filtered out above with the help of `option`
                      .lhs_fields, // existing relationship would handle the legacy/ new shape, ie. lhs_fields will always be present (legacy hasura_field is already translated to the new format)
                    remote_field: (existingRelationship.relationship as DbToRemoteSchemaRelationship)
                      .remote_field,
                    remote_schema: (existingRelationship.relationship as DbToRemoteSchemaRelationship)
                      .remoteSchemaName,
                  },
                },
              }
            }
          />
        )}
      </div>
    </div>
  );
};
