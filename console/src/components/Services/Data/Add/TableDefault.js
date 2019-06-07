import React from 'react';
import CustomInputAutoSuggest from '../../../Common/CustomInputAutoSuggest/CustomInputAutoSuggest';
import {
  getPlaceholder,
  getDefaultValue,
  getDefaultFunctionsOptions,
} from '../Common/utils';

const TableDefault = ({
  column,
  colDefaultFunctions,
  onChange,
  testId,
  colIndex: i,
}) => {
  // const styles = require('../../../Common/TableCommon/Table.scss');
  const defaultInputTheme = require('../../../Common/CustomInputAutoSuggest/CustomInputAddTableTheme.scss');
  const handleColDefaultValueChange = (e, data) => {
    const { newValue } = data;
    onChange(i, column.nullable || false, newValue);
  };
  const renderDefaultHtml = () => {
    const dfVal = getDefaultValue(column);
    /* Collect direct default functions and the indirect default functions */
    const {
      defaultValues,
      // defaultValueMap
    } = getDefaultFunctionsOptions(colDefaultFunctions, i);
    return (
      <CustomInputAutoSuggest
        options={defaultValues}
        onChange={handleColDefaultValueChange}
        value={dfVal}
        className={`col-default-value-${i} add_table_default_value_selector form-control`}
        placeholder={getPlaceholder(column)}
        id={`col-default-value-${i}`}
        theme={defaultInputTheme}
        data-test={testId}
      />
    );
    /*
    return (
      <input
        placeholder={getPlaceholder(column)}
        type="text"
        value={getDefaultValue(column)}
        className="form-control"
        onChange={e => onChange(i, column.nullable || false, e.target.value)}
        data-test={testId}
      />
    );
    */
  };
  return renderDefaultHtml();
};

export default TableDefault;
