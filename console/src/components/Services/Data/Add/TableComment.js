import React from 'react';
import PropTypes from 'prop-types';

import { Heading } from '../../../UIKit/atoms';
import TextInput from '../../../Common/TextInput/TextInput';
import styles from '../../../Common/TableCommon/Table.scss';

const TableComment = ({ onChange }) => {
  return [
    <Heading key="add_table_comment_header" type="subHeading" mr="sm">
      Comment
    </Heading>,
    <TextInput
      key="add_table_comment_element"
      type="text"
      placeholder="comment"
      data-test="tableComment"
      bsclass={`${styles.tableNameInput}`}
      onChange={onChange}
    />,
  ];
};

TableComment.propTypes = {
  onChange: PropTypes.func.isRequired,
};

export default TableComment;
