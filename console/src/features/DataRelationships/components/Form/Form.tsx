import { Button } from '@/new-components/Button';
import React, { useState } from 'react';
import { CardRadioGroup } from '@/new-components/CardRadioGroup';
import { LocalRelationshipWidget } from '../LocalDBRelationshipWidget';

type Value =
  | 'Local Relationship'
  | 'Remote Database Relationship'
  | 'Remote Schema Relationship';

const data: { value: Value; title: string; body: string }[] = [
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

export const Form = () => {
  const [option, setOption] = useState('Local Relationship');
  return (
    <div className="w-full sm:w-9/12 bg-white shadow-sm rounded p-md border border-gray-300 shadow show">
      <div className="flex items-center mb-md">
        <Button size="sm">Cancel</Button>
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
        />
        {option === 'Local Relationship' ? (
          <LocalRelationshipWidget
            sourceTableInfo={{
              database: 'default',
              schema: 'public',
              table: 'resident',
            }}
          />
        ) : option === 'Remote Database Relationship' ? (
          <>do something for remote DB relationships</>
        ) : (
          <>do something for remote schema relationships</>
        )}
      </div>
    </div>
  );
};
