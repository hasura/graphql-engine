import React from 'react';

import PropTypes from 'prop-types';

import TextInput from '../../../Common/TextInput/TextInput';

const TableNameInput = ({ onChange }) => {
  return [
    <TextInput
      key="add_table_input_element"
      type="text"
      placeholder="table_name"
      data-test="tableName"
      onChange={onChange}
    />,
  ];
};

TableNameInput.propTypes = {
  onChange: PropTypes.func.isRequired,
};

export default TableNameInput;
