import React from 'react';

import PropTypes from 'prop-types';

import TextInput from '../../../Common/TextInput/TextInput';

const TableComment = ({ onChange }) => {
  const styles = require('../../../Common/TableCommon/Table.scss');
  return [
    <h4 key="add_table_comment_header" className={styles.subheading_text}>
      Comment &nbsp; &nbsp;
    </h4>,
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
