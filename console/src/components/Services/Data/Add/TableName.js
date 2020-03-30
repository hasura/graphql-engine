import React from 'react';

import PropTypes from 'prop-types';
import TextInput from '../../../Common/TextInput/TextInput';
import { Heading } from '../../../UIKit/atoms';
import styles from '../../../Common/TableCommon/Table.scss';

const TableNameInput = ({ onChange }) => {
  return [
    <Heading
      as="h4"
      key="add_table_input_header"
      fontSize="15px"
      pb="20px"
      mt="0px"
      mb="0px"
      ml="sm"
    >
      Table Name
    </Heading>,
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
