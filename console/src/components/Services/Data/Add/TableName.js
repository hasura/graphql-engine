import React from 'react';

import PropTypes from 'prop-types';

import TextInput from '../../../Common/TextInput/TextInput';

const TableNameInput = ({ onChange }) => {
  const styles = require('../../../Common/TableCommon/Table.scss');
  return [
    <h4 key="add_table_input_header" className={styles.subheading_text}>
      Table Name &nbsp; &nbsp;
    </h4>,
    <TextInput
      key="add_table_input_element"
      type="text"
      placeholder="table_name"
      data-test="tableName"
      bsclass={`${styles.tableNameInput}`}
      onChange={onChange}
    />,
  ];
};

TableNameInput.propTypes = {
  onChange: PropTypes.func.isRequired,
};

export default TableNameInput;
