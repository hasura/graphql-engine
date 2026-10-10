import React, { useEffect, useRef, useState } from 'react';
import omit from 'lodash/omit';
import { Button, Dialog, DialogFooter, Input, Text } from '@hasura/shared/ui';
import { FaEdit } from 'react-icons/fa';
import { MetadataFunction, QualifiedDataSource } from '@hasura/shared/types';
import { Flex } from '@radix-ui/themes';
import { useSetFunctionConfiguration } from '../../hooks/useSetFunctionConfiguration';
import { LoadDatabaseCommentButton } from '../../components/LoadDatabaseCommentButton';

interface FunctionCommentEditorProps {
  defaultValue: string;
  readOnly: boolean;
  source: QualifiedDataSource;
  func: MetadataFunction;
  onSuccess?: () => void;
}

export const FunctionCommentEditor: React.FC<FunctionCommentEditorProps> = (
  props,
) => {
  const [isEditing, setEditing] = useState(false);

  // Identifies which function/source the dialog is currently editing a
  // comment for. If either changes while the dialog is open (e.g. the user
  // selects a different function in the table list), any in-progress draft
  // no longer applies to what's on screen, so the dialog should close
  // rather than silently keep editing (and potentially save) stale data.
  const editingTarget = JSON.stringify({
    func: props.func,
    source: props.source,
  });
  const previousEditingTarget = useRef(editingTarget);

  useEffect(() => {
    if (previousEditingTarget.current !== editingTarget) {
      previousEditingTarget.current = editingTarget;
      setEditing(false);
    }
  }, [editingTarget]);

  const openEditor = () => {
    setEditing(true);
  };

  const commentEditCancel = () => {
    setEditing(false);
  };

  return (
    <div>
      <Flex className="mb-2" align="center" gap="2">
        <Text weight="bold">Function Comments</Text>
        {!props.readOnly && (
          <Button
            size="1"
            mode="default"
            leftIcon={FaEdit}
            onClick={openEditor}
            aria-label={props.defaultValue ? 'Edit comment' : 'Add a comment'}
          >
            {props.defaultValue ? 'Edit' : 'Add a Comment'}
          </Button>
        )}
      </Flex>
      {props.defaultValue && <Text as="div">{props.defaultValue}</Text>}

      {isEditing && (
        <FunctionCommentDialog {...props} onCancel={commentEditCancel} />
      )}
    </div>
  );
};

const FunctionCommentDialog: React.FC<
  FunctionCommentEditorProps & {
    onCancel: () => void;
  }
> = ({ defaultValue, source, func, onSuccess, onCancel }) => {
  const [comment, setComment] = useState(defaultValue);
  const { setFunctionConfiguration, isPending: isLoading } =
    useSetFunctionConfiguration({ dataSourceName: source.name });

  const commentEditSave = () => {
    // `set_function_customization` replaces the whole configuration, so keep
    // the existing fields and only change the comment.
    const rest = omit(func.configuration ?? {}, 'comment');
    const trimmed = comment.trim();
    setFunctionConfiguration({
      qualifiedFunction: func.function,
      configuration: trimmed ? { ...rest, comment: trimmed } : rest,
      onSuccess: () => {
        onSuccess?.();
        onCancel();
      },
    });
  };

  return (
    <Dialog
      size="sm"
      title={defaultValue ? 'Edit Comment' : 'Add a Comment'}
      onClose={onCancel}
      onOpenChange={(open) => {
        if (!open) {
          onCancel();
        }
      }}
    >
      <form
        onSubmit={(event) => {
          event.preventDefault();
          commentEditSave();
        }}
      >
        <Input
          autoFocus
          onChange={(event) => setComment(event.target.value)}
          type="text"
          value={comment}
          placeholder="Function comment..."
          aria-label="Function comment"
        />
        <Flex justify="end" className="mt-2">
          <LoadDatabaseCommentButton
            source={source}
            target={{ type: 'function', func: func.function }}
            onLoad={setComment}
          />
        </Flex>
        <DialogFooter
          callToAction="Save"
          callToActionProps={{
            loadingText: 'Saving...',
          }}
          callToDeny="Cancel"
          onClose={onCancel}
          isLoading={isLoading}
        />
      </form>
    </Dialog>
  );
};

export default FunctionCommentEditor;
