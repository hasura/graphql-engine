import React from 'react';
import {
  activateCommentEdit,
  updateCommentInput,
  saveTableCommentSql,
} from './ModifyActions';

const TableCommentEditor = ({
  tableComment,
  tableCommentEdit,
  tableType,
  dispatch,
  readOnly,
}) => {
  const editCommentClicked = () => {
    dispatch(activateCommentEdit(true, tableComment));
  };

  const commentEdited = e => {
    dispatch(updateCommentInput(e.target.value));
  };

  const commentEditSave = () => {
    dispatch(saveTableCommentSql(tableType));
  };

  const commentEditCancel = () => {
    dispatch(activateCommentEdit(false, ''));
  };

  let commentHtml = readOnly ? null : (
    <div
      onClick={editCommentClicked}
      className="flex items-center cursor-pointer"
    >
      <i className="fa fa-edit mr-xs" />
      <span className="font-semibold">Add a Comment</span>
    </div>
  );

  if (tableComment && !tableCommentEdit.enabled) {
    commentHtml = (
      <div>
        <textarea
          className="w-100 rounded bg-secondary-light border border-gray-300 border-l-4 border-l-secondary py-sm px-md mb-sm"
          readOnly
        >
          {tableComment}
        </textarea>
        {!readOnly && (
          <div
            onClick={editCommentClicked}
            className="flex items-center cursor-pointer"
          >
            <i className="fa fa-edit mr-xs" />
            <span className="font-semibold">Edit Comment</span>
          </div>
        )}
      </div>
    );
  } else if (tableCommentEdit.enabled) {
    commentHtml = (
      <div className="flex items-center">
        <textarea
          onChange={commentEdited}
          className="form-control"
          type="text"
          defaultValue={tableComment}
        >
          {tableCommentEdit.value}
        </textarea>
        <div onClick={commentEditSave} className="ml-sm cursor-pointer">
          Save
        </div>
        <div onClick={commentEditCancel} className="ml-sm cursor-pointer">
          Cancel
        </div>
      </div>
    );
  }

  return commentHtml;
};

export default TableCommentEditor;
