import { useUpdateEffect } from '@hasura/shared/hooks';
import { Button, TextArea } from '@hasura/shared/ui';
import React from 'react';
import { useUpdateTableConfiguration } from '../hooks';
import { ModifyTableProps } from '../types';
import { Flex } from '@radix-ui/themes';
import { Section } from '../parts';
import { FaRegComment } from 'react-icons/fa6';
import { LoadDatabaseCommentButton } from '../../components/LoadDatabaseCommentButton';

type TableCommentsProps = ModifyTableProps;

/** Edits the metadata `configuration.comment` of a table or view. */
export const TableComments: React.FC<TableCommentsProps> = ({
  source,
  table,
  isView,
}) => {
  const savedComment = table.configuration?.comment;
  const [comment, setComment] = React.useState(savedComment);

  useUpdateEffect(() => {
    // this resets the state so we aren't carrying over the comment from a previous table
    // prevents the saveNeeded from becoming inaccurate
    setComment(savedComment);
  }, [table]);

  const saveNeeded = comment != null && comment !== savedComment;

  const { updateTableConfiguration, isPending: savingComment } =
    useUpdateTableConfiguration(source.name, table.table);

  return (
    <Section
      headerText={
        <Flex align="center" gap="2">
          {isView ? 'View Comments' : 'Table Comments'}
          <FaRegComment />
          <LoadDatabaseCommentButton
            source={source}
            target={
              isView
                ? { type: 'view', table: table.table }
                : { type: 'table', table: table.table }
            }
            onLoad={setComment}
          />
          {saveNeeded && (
            <Button
              loadingText="Saving"
              loading={savingComment}
              size="1"
              onClick={() => {
                if (comment != null) {
                  updateTableConfiguration({ comment });
                }
              }}
            >
              Save
            </Button>
          )}
        </Flex>
      }
    >
      <TextArea
        className="w-full"
        name="comments"
        value={comment ?? ''}
        onChange={(e) => setComment(e.currentTarget.value)}
        placeholder="Add a comment to display here"
      />
    </Section>
  );
};
