import { Button } from '@/new-components/Button';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { Nullable } from '@/types';
import clsx from 'clsx';
import React, { useMemo } from 'react';
import TextareaAutosize from 'react-autosize-textarea';
import { FaEdit } from 'react-icons/fa';
import { ManageTableProps } from '../../ManageTable';
import { useMetadataForManageTable, getTableFromMetadata } from '../hooks';
import { useUpdateTableConfiguration } from '../hooks/useUpdateTableConfiguration';

export const TableComments: React.VFC<ManageTableProps> = props => {
  const { dataSourceName, table } = props;
  const { data, isLoading } = useMetadataForManageTable(dataSourceName);

  const savedComment = React.useMemo(
    () => getTableFromMetadata(data?.metadata, table)?.configuration?.comment,
    [table, data?.metadata]
  );

  const [comment, setComment] = React.useState<Nullable<string>>(null);

  const saveNeeded = useMemo(
    () => comment != null && comment !== savedComment,
    [comment, savedComment]
  );

  const { updateTableConfiguration, isLoading: savingComment } =
    useUpdateTableConfiguration(dataSourceName, table);

  if (isLoading) return <IndicatorCard status="info">Loading...</IndicatorCard>;

  return (
    <div className="mb-lg flex items-center">
      <div className="relative">
        <TextareaAutosize
          style={{ minWidth: '24rem', outline: 'none' }}
          rows={1}
          className={clsx(
            'bg-secondary-light border border-gray-300 border-l-4 border-l-secondary p-sm peer',
            'focus:bg-white rounded focus:[box-shadow:none] focus:border-secondary',
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
            'invisible absolute opacity-50 left-0 top-1/2 -translate-y-1/2 -mt-[4px] ml-4',
            'peer-placeholder-shown:visible'
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
              updateTableConfiguration({ comment });
            }
          }}
        >
          Save
        </Button>
      )}
    </div>
  );
};
