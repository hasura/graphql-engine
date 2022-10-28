import { Tooltip } from '@/new-components/Tooltip';
import React, { useState } from 'react';
import clsx from 'clsx';
import { FaPlus, FaQuestionCircle } from 'react-icons/fa';
import { Table } from '@/features/MetadataAPI';
import { Button } from '@/new-components/Button';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { CardRadioGroup } from '@/new-components/CardRadioGroup';
import {
  DatabaseRelationshipsTable,
  DatabaseRelationshipsTableProps,
} from '../DatabaseRelationshipsTable';
import { ManualLocalRelationshipWidget } from '../ManualLocalRelationshipWidget/ManualLocalRelationshipWidget';
import { Relationship } from '../DatabaseRelationshipsTable/types';
import { useDeleteRelationship } from '../../hooks/useDeleteRelationships';

interface DatabaseRelationshipsContainerProps {
  dataSourceName: string;
  table: Table;
}

const typesOfRelationships = [
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

/**
 * Please note that is a modified version of the DatabaseRelationshipsTab but with less features in it and made to work only for GDC
 * and anything under the data/v2/ route of the console
 */
export const DatabaseRelationshipsContainer = (
  props: DatabaseRelationshipsContainerProps
) => {
  const [selectedRelationship, setSelectedRelationship] =
    useState<Relationship>();

  const { deleteRelationship } = useDeleteRelationship();

  const [isFormOpen, setIsFormOpen] = useState(false);
  const [selectedOption, setSelectedOption] = useState('local');

  const editRowHandler: DatabaseRelationshipsTableProps['onEditRow'] =
    values => {
      setIsFormOpen(true);
      setSelectedRelationship(values.relationship);

      if (
        values.relationship.type === 'toLocalTableFk' ||
        values.relationship.type === 'toLocalTableManual' ||
        values.relationship.type === 'toSameTableFk'
      )
        setSelectedOption('local');
      else if (values.relationship.type === 'toRemoteSchema')
        setSelectedOption('remoteSchema');
      else setSelectedOption('remoteDatabase');
    };

  const deleteRowHandler: DatabaseRelationshipsTableProps['onDeleteRow'] =
    values => {
      deleteRelationship(
        values.dataSourceName,
        values.table,
        values.relationship
      );
    };

  const resetForm = () => {
    setSelectedRelationship(undefined);
    setIsFormOpen(false);
  };

  const openEmptyForm = () => {
    setSelectedRelationship(undefined);
    setIsFormOpen(true);
  };

  return (
    <div>
      <div className="my-4 text-gray-600 flex">
        <span className="text-2xl font-semibold">Data Relationships</span>
        <Tooltip tooltipContentChildren="List of all your table's relationships">
          <span>
            <FaQuestionCircle />
          </span>
        </Tooltip>
      </div>

      <DatabaseRelationshipsTable
        {...props}
        onDeleteRow={deleteRowHandler}
        onEditRow={editRowHandler}
        key={`${props.dataSourceName}-${props.table}`}
      />

      {isFormOpen && (
        <div className="w-full sm:w-9/12 bg-white shadow-sm rounded p-md border border-gray-300 show">
          <div className="flex items-center mb-md justify-between">
            <span className="text-lg">
              {selectedRelationship ? (
                <span className="font-semibold">
                  Editing relationship :{' '}
                  <span className="font-bold">{selectedRelationship.name}</span>
                </span>
              ) : (
                <span className="font-bold">Create a new Relationship</span>
              )}
            </span>
            <Button size="sm" onClick={resetForm}>
              Cancel
            </Button>
          </div>
          <hr className="mb-md border-gray-300" />
          <div>
            <div className={clsx(selectedRelationship ? 'hidden' : 'normal')}>
              <CardRadioGroup
                items={typesOfRelationships}
                onChange={setSelectedOption}
                value={selectedOption}
              />
            </div>

            {selectedOption === 'local' ? (
              <ManualLocalRelationshipWidget
                dataSourceName={props.dataSourceName}
                table={props.table}
                onSuccess={resetForm}
                existingRelationship={selectedRelationship}
                key={selectedRelationship?.name ?? 'new-relationship'}
              />
            ) : (
              <div>
                <IndicatorCard headline="Feature is currently unavailable">
                  Feature not implemented
                </IndicatorCard>
              </div>
            )}
          </div>
        </div>
      )}

      {!isFormOpen && (
        <Button icon={<FaPlus />} onClick={openEmptyForm}>
          Add New Relationship
        </Button>
      )}
    </div>
  );
};
