import { Source } from '@/features/MetadataAPI';
import { Button } from '@/new-components/Button';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { Nullable } from '@/types';
import clsx from 'clsx';
import isEqual from 'lodash.isequal';
import React, { useMemo } from 'react';
import TextareaAutosize from 'react-autosize-textarea';
import { FaEdit } from 'react-icons/fa';
import { ManageTableProps } from '../../ManageTable';
import { useMetadataForManageTable, useUpdateComment } from '../hooks';

const getTableFromMetadata = (metadata: Source | undefined, table: unknown) => {
  if (!table) return null;
  return metadata?.tables.find(t => isEqual(t.table, table));
};

export const TableComments: React.VFC<ManageTableProps> = props => {
  const { dataSourceName, table } = props;
  const { data: metadata, isLoading } =
    useMetadataForManageTable(dataSourceName);

  const savedComment = React.useMemo(
    () => getTableFromMetadata(metadata, table)?.configuration?.comment,
    [table, metadata]
  );
  const [comment, setComment] = React.useState<Nullable<string>>(null);

  const saveNeeded = useMemo(
    () => comment != null && comment !== savedComment,
    [comment, savedComment]
  );

  const { updateComment, isLoading: savingComment } = useUpdateComment(
    dataSourceName,
    table
  );

  if (isLoading) return <IndicatorCard status="info">Loading...</IndicatorCard>;

  return (
    <div className="mb-lg flex items-center">
      <div className="relative">
        <TextareaAutosize
          style={{ minWidth: '24rem', outline: 'none' }}
          rows={1}
          className={clsx(
            'bg-secondary-light border border-gray-300 border-l-4 border-l-secondary p-sm peer',
            'focus:bg-white rounded focus:[box-shadow:none]',
            'placeholder-shown:italic placeholder-shown:pl-8',
            saveNeeded && 'border-l-red-500 '
          )}
          name="comments"
          defaultValue={savedComment}
          onChange={e => setComment(e.currentTarget.value)}
          placeholder="Add a comment to display here"
        />
        <FaEdit
          className={clsx(
            'hidden absolute opacity-50 left-0 top-1/2 -translate-y-1/2 -mt-[4px] ml-4',
            'peer-placeholder-shown:inline'
          )}
        />
      </div>
      {saveNeeded && (
        <Button
          loadingText="Saving"
          isLoading={savingComment}
          className="ml-sm"
          onClick={() => {
            if (comment != null) {
              updateComment(comment);
            }
          }}
        >
          Save
        </Button>
      )}
    </div>
  );
};
