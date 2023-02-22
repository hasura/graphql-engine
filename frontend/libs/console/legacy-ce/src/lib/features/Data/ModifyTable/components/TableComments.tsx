import { MetadataUtils, useMetadata } from '../../../hasura-metadata-api';
import { Table } from '../../../hasura-metadata-types';
import useUpdateEffect from '../../../../hooks/useUpdateEffect';
import { Button } from '../../../../new-components/Button';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { Nullable } from '../../../../types';
import clsx from 'clsx';
import React from 'react';
import TextareaAutosize from 'react-autosize-textarea';
import { FaEdit, FaRegComment } from 'react-icons/fa';
import Skeleton from 'react-loading-skeleton';
import { useUpdateTableConfiguration } from '../hooks';

import { ModifyTableProps } from '../ModifyTable';

export interface TableCommentsProps extends ModifyTableProps {
  dataSourceName: string;
  table: Table;
}

export const TableComments: React.VFC<TableCommentsProps> = props => {
  const { dataSourceName, table } = props;

  const {
    isLoading,
    data: savedComment,
    isError,
  } = useMetadata(
    m =>
      MetadataUtils.findMetadataTable(dataSourceName, table, m)?.configuration
        ?.comment
  );

  const [comment, setComment] = React.useState<Nullable<string>>(null);

  useUpdateEffect(() => {
    // this resets the state so we aren't carrying over the comment from a previous table
    // prevents the saveNeeded from becoming inaccurate
    setComment(null);
  }, [table]);

  const saveNeeded = comment != null && comment !== savedComment;

  const { updateTableConfiguration, isLoading: savingComment } =
    useUpdateTableConfiguration(dataSourceName, table);

  if (isLoading) return <Skeleton count={5} height={20} />;

  if (isError)
    return (
      <IndicatorCard status="negative" headline="Error">
        Unable to fetch table comments.
      </IndicatorCard>
    );

  return (
    <div className="mb-lg flex items-center">
      <div className="relative">
        <TextareaAutosize
          style={{ minWidth: '24rem', outline: 'none' }}
          rows={1}
          key={savedComment}
          className={clsx(
            'bg-secondary-light border border-gray-300 border-l-4 border-l-secondary p-sm peer',
            'focus:bg-white rounded focus:[box-shadow:none] focus:border-secondary',
            'placeholder-shown:italic pl-9'
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
        <FaRegComment
          className={clsx(
            'absolute opacity-50 left-0 top-1/2 -translate-y-1/2 -mt-[4px] ml-4',
            'peer-placeholder-shown:invisible'
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
