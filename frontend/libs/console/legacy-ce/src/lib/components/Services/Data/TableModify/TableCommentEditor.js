import React from 'react';
import { FaEdit } from 'react-icons/fa';
import { inputStyles } from '../constants';
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
      <FaEdit className="mr-xs" />
      <span className="font-semibold">Add a Comment</span>
    </div>
  );

  if (tableComment && !tableCommentEdit.enabled) {
    commentHtml = (
      <div>
        <div className="rounded bg-secondary-light border border-gray-300 border-l-4 border-l-secondary py-sm px-md mb-sm">
          {tableComment}
        </div>
        {!readOnly && (
          <div
            onClick={editCommentClicked}
            className="flex items-center cursor-pointer"
          >
            <FaEdit className="mr-xs" />
            <span className="font-semibold">Edit Comment</span>
          </div>
        )}
      </div>
    );
  } else if (tableCommentEdit.enabled) {
    commentHtml = (
      <div className="flex items-center">
        <input
          onChange={commentEdited}
          className={inputStyles}
          type="text"
          value={tableCommentEdit.value}
          defaultValue={tableComment}
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

  return commentHtml;
};

export default TableCommentEditor;
