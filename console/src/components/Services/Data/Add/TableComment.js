import React from 'react';

import PropTypes from 'prop-types';

const TableComment = ({ onChange }) => {
  return [
    <textarea
      key="add_table_comment_element"
      type="text"
      placeholder="comment"
      data-test="tableComment"
      onChange={onChange}
      className="form-control"
    />,
  ];
};

TableComment.propTypes = {
  onChange: PropTypes.func.isRequired,
};

export default TableComment;
