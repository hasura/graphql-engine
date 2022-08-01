import React from 'react';
import { FaArrowRight, FaColumns, FaMagic, FaTable } from 'react-icons/fa';
import { CardedTable } from '@/new-components/CardedTable';
import { Button } from '@/new-components/Button';

import { DataTarget } from '@/features/Datasources';

import { useSuggestedRelationships } from './hooks';

export interface SuggestedRelationshipProps {
  target: DataTarget;
}

export const SuggestedRelationships = ({
  target,
}: SuggestedRelationshipProps) => {
  const { data, isLoading, isError } = useSuggestedRelationships(target);

  if (isError) {
    return (
      <p className="text-red-500">Error loading suggested relationships</p>
    );
  }

  if (isLoading) {
    return <p>Loading...</p>;
  }

  if (!data?.length) {
    return <p>No Suggested relationships</p>;
  }

  return (
    <div>
      <CardedTable.Table>
        <CardedTable.TableHead>
          <CardedTable.TableHeadRow>
            <CardedTable.TableHeadCell>
              <FaMagic className="text-sm text-gray-500 mr-2" />
              SUGGESTED RELATIONSHIPS
            </CardedTable.TableHeadCell>
            <CardedTable.TableHeadCell>TYPE</CardedTable.TableHeadCell>
            <CardedTable.TableHeadCell>RELATIONSHIP</CardedTable.TableHeadCell>
          </CardedTable.TableHeadRow>
        </CardedTable.TableHead>

        <CardedTable.TableBody>
          {data?.map(relationship => (
            <CardedTable.TableBodyRow
              key={`${relationship.to}-${relationship.from}`}
            >
              <CardedTable.TableBodyCell>
                <Button
                  size="sm"
                  onClick={() =>
                    console.log(
                      'in the future ill open a form, but now i nothing :('
                    )
                  }
                >
                  Add
                </Button>
              </CardedTable.TableBodyCell>
              <CardedTable.TableBodyCell>
                <FaTable className="text-sm text-muted mr-xs" />
                Local Relation
              </CardedTable.TableBodyCell>
              <CardedTable.TableBodyCell>
                <span className="capitalize">{relationship.type}</span>
              </CardedTable.TableBodyCell>
              <CardedTable.TableBodyCell>
                <FaTable className="text-sm text-muted mr-xs" />
                {relationship.from.table}&nbsp;/&nbsp;
                <FaColumns className="text-sm text-muted mr-xs" />
                {relationship.from.column}
              </CardedTable.TableBodyCell>
              <CardedTable.TableBodyCell>
                <FaArrowRight className="fill-current text-sm text-muted" />
              </CardedTable.TableBodyCell>
              <CardedTable.TableBodyCell>
                <FaTable className="text-sm text-muted mr-xs" />
                {relationship.to.table}&nbsp;/&nbsp;
                <FaColumns className="text-sm text-muted mr-xs" />
                {relationship.to.column}
              </CardedTable.TableBodyCell>
            </CardedTable.TableBodyRow>
          ))}
        </CardedTable.TableBody>
      </CardedTable.Table>
    </div>
  );
};
