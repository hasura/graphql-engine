import React from 'react';
import { Button } from '../../../../../new-components/Button';
import { FaEdit } from 'react-icons/fa';

import { saveFunctionComment, setEditing } from '../customFunctionReducer';

interface FunctionCommentEditorProps {
  isEditing: boolean;
  defaultValue: string;
  readOnly: boolean;
  dispatch: (input: any) => void;
}

export const FunctionCommentEditor: React.FC<FunctionCommentEditorProps> = ({
  isEditing,
  defaultValue,
  readOnly,
  dispatch,
}) => {
  const [comment, setComment] = React.useState('');

  const dispatchIsEditing = React.useCallback(
    (editing: boolean) => {
      dispatch(setEditing(editing));
    },
    [dispatch]
  );

  const commentEditSave = () => {
    dispatch(saveFunctionComment(comment));
  };

  const commentEditCancel = () => {
    setComment(defaultValue);
    dispatchIsEditing(false);
  };

  React.useEffect(() => {
    setComment(defaultValue);
  }, [defaultValue]);

  if (isEditing) {
    return (
      <div className="flex items-center">
        <input
          onChange={event => setComment(event.target.value)}
          className="form-control"
          type="text"
          value={comment}
          placeholder="Function comment..."
        />
        <div onClick={commentEditSave} className="ml-sm cursor-pointer">
          Save
        </div>
        <div onClick={commentEditCancel} className="ml-sm cursor-pointer">
          Cancel
        </div>
      </div>
    );
  }

  return (
    <div>
      {comment && (
        <div className="rounded bg-secondary-light border border-gray-300 border-l-4 border-l-secondary py-sm px-md mb-sm">
          {comment}
        </div>
      )}
      {!readOnly && (
        <Button icon={<FaEdit />} onClick={() => dispatchIsEditing(true)}>
          {comment ? 'Edit Comment' : 'Add a Comment'}
        </Button>
      )}
    </div>
  );
};

export default FunctionCommentEditor;
