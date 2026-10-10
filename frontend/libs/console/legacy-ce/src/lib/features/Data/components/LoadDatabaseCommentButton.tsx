import { FaDatabase } from 'react-icons/fa';
import { Button, hasuraToast } from '@hasura/shared/ui';
import { QualifiedDataSource } from '@hasura/shared/types';
import {
  DatabaseCommentTarget,
  useDatabaseComment,
} from '@hasura/metadata/data-source';

type LoadDatabaseCommentButtonProps = {
  source: QualifiedDataSource;
  target: DatabaseCommentTarget;
  /** Called with the database comment; the caller decides how to apply it. */
  onLoad: (comment: string) => void;
};

/**
 * Fills a metadata comment field from the object's database (SQL) comment.
 * Renders nothing when the driver can't read that kind of comment.
 */
export const LoadDatabaseCommentButton = ({
  source,
  target,
  onLoad,
}: LoadDatabaseCommentButtonProps) => {
  const { isSupported, fetchComment, isFetching } = useDatabaseComment({
    source,
    target,
  });

  if (!isSupported) return null;

  const onClick = async () => {
    try {
      const comment = await fetchComment();
      if (comment) {
        onLoad(comment);
      } else {
        hasuraToast({
          type: 'info',
          title: 'No database comment',
          message: `This ${target.type} has no comment in the database.`,
        });
      }
    } catch (err) {
      hasuraToast({
        type: 'error',
        title: 'Failed to load the database comment',
        message: err instanceof Error ? err.message : String(err),
      });
    }
  };

  return (
    <Button
      type="button"
      size="1"
      mode="default"
      leftIcon={FaDatabase}
      loading={isFetching}
      loadingText="Loading"
      onClick={onClick}
    >
      Load from database
    </Button>
  );
};
