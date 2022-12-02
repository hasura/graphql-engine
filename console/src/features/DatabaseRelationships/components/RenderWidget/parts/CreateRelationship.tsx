import { Table } from '@/features/hasura-metadata-types';
import { CardRadioGroup } from '@/new-components/CardRadioGroup';
import { Dialog } from '@/new-components/Dialog';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import React, { useState } from 'react';
import { ManualLocalRelationship } from '../../ManualLocalRelationship';

interface CreateRelationshipProps {
  dataSourceName: string;
  table: Table;
  onCancel: () => void;
  onSuccess: () => void;
  onError: (err: Error) => void;
}

const formTabs = [
  {
    value: 'local',
    title: 'Local Relationship',
    body: 'Relationships from this table to a local database table.',
  },
  {
    value: 'remoteDatabase',
    title: 'Remote Database Relationship',
    body: 'Relationship from this local table to a remote database table.',
  },
  {
    value: 'remoteSchema',
    title: 'Remote Schema Relationship',
    body: 'Relationship from this local table to a remote schema.',
  },
];

export const CreateRelationship = (props: CreateRelationshipProps) => {
  const { dataSourceName, table, onCancel, onSuccess, onError } = props;

  const [relationshipType, setRelationshipType] = useState('local');

  return (
    <Dialog
      hasBackdrop
      title="Create New Relationship"
      description="Create and track a new relationship to view it in your GraphQL schema."
      onClose={onCancel}
      size="max"
    >
      <div className="mx-4">
        <CardRadioGroup
          items={formTabs}
          onChange={setRelationshipType}
          value={relationshipType}
        />
        {relationshipType === 'local' && (
          <ManualLocalRelationship.Widget
            dataSourceName={dataSourceName}
            table={table}
            onSuccess={onSuccess}
            onError={onError}
            onCancel={onCancel}
          />
        )}
        {(relationshipType === 'remoteDatabase' ||
          relationshipType === 'remoteSchema') && (
          <div className="mt-sm">
            <IndicatorCard status="info" headline="Feature coming soon" />
          </div>
        )}
      </div>
    </Dialog>
  );
};
