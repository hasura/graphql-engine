import React from 'react';
import {
  activateCommentEdit,
  updateCommentInput,
  saveTableCommentSql,
} from './ModifyActions';

import styles from './ModifyTable.scss';

const TableCommentEditor = ({
  tableComment,
  tableCommentEdit,
  isTable,
  dispatch,
}) => {
  const editCommentClicked = () => {
    dispatch(activateCommentEdit(true, tableComment));
  };

  const commentEdited = e => {
    dispatch(updateCommentInput(e.target.value));
  };

  const commentEditSave = () => {
    dispatch(saveTableCommentSql(isTable));
  };

  const commentEditCancel = () => {
    dispatch(activateCommentEdit(false, ''));
  };

  let commentHtml = (
    <div className={styles.add_pad_bottom}>
      <div className={styles.commentText}>Add a comment</div>
      <div onClick={editCommentClicked} className={styles.commentEdit}>
        <i className="fa fa-edit" />
      </div>
    </div>
  );

  if (tableComment && !tableCommentEdit.enabled) {
    commentHtml = (
      <div className={styles.mar_bottom}>
        <div className={styles.commentText + ' alert alert-warning'}>
          {tableComment}
        </div>
        <div onClick={editCommentClicked} className={styles.commentEdit}>
          <i className="fa fa-edit" />
        </div>
      </div>
    );
  } else if (tableCommentEdit.enabled) {
    commentHtml = (
      <div className={styles.mar_bottom}>
        <input
          onChange={commentEdited}
          className={'form-control ' + styles.commentInput}
          type="text"
          value={tableCommentEdit.value}
          defaultValue={tableComment}
        />
        <div
          onClick={commentEditSave}
          className={
            styles.display_inline +
            ' ' +
            styles.add_pad_left +
            ' ' +
            styles.comment_action
          }
        >
          Save
        </div>
        <div
          onClick={commentEditCancel}
          className={
            styles.display_inline +
            ' ' +
            styles.add_pad_left +
            ' ' +
            styles.comment_action
          }
        >
          Cancel
        </div>
      </div>
    );
  }

  return commentHtml;
};

export default TableCommentEditor;
