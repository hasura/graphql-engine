import React from 'react';

import PropTypes from 'prop-types';

import TextInput from '../../../Common/TextInput/TextInput';

const TableComment = ({ onChange }) => {
  return [
    <TextInput
      key="add_table_comment_element"
      type="text"
      placeholder="comment"
      data-test="tableComment"
      onChange={onChange}
    />,
  ];
};

TableComment.propTypes = {
  onChange: PropTypes.func.isRequired,
};

export default TableComment;
