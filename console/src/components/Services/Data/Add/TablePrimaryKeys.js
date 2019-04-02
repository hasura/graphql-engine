import React from 'react';
import PropTypes from 'prop-types';
import * as tooltip from './Tooltips';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';

const styles = require('../../../Common/TableCommon/Table.scss');

const getPrimaryKeys = (columns, primaryKeys, onRemove, onChange) => {
  return primaryKeys.map((pk, i) => {
    let removeIcon;
    if (i + 1 === primaryKeys.length) {
      removeIcon = null;
    } else {
      removeIcon = (
        <i
          className={`${styles.fontAwosomeClose} fa-lg fa fa-times`}
          onClick={onRemove.bind(undefined, i)}
        />
      );
    }
    /* When the primary key option is not selected, show the disabled by default */
    const getDisabledOption = () => {
      return pk === '' ? (
        <option disabled value="">
          -- select --
        </option>
      ) : null;
    };
    const getPrimaryKeyOptions = () => {
      return columns
        .filter(({ name }) => name.length > 0)
        .map(({ name }, j) => (
          <option key={j} value={j}>
            {name}
          </option>
        ));
    };
    return (
      <div key={i} className="form-group">
        <select
          value={pk || ''}
          className={`${styles.select} form-control ${styles.add_pad_left}`}
          onChange={onChange.bind(undefined, i)}
          data-test={`primary-key-select-${i}`}
          data-test={`primary-key-select-${i.toString()}`}
        >
          {getDisabledOption()}
          {getPrimaryKeyOptions()}
        </select>
        {removeIcon}
      </div>
    );
  });
};

const TablePrimaryKeys = ({ columns, primaryKeys, onRemove, onChange }) => {
  return [
    <h4 key="add_table_primary_key_header" className={styles.subheading_text}>
      Primary Key &nbsp; &nbsp;
      <OverlayTrigger placement="right" overlay={tooltip.primaryKeyDescription}>
        <i className="fa fa-question-circle" aria-hidden="true" />
      </OverlayTrigger>{' '}
      &nbsp; &nbsp;
    </h4>,
    <div key="add_table_primary_key_body">
      {getPrimaryKeys(columns, primaryKeys, onRemove, onChange)}
    </div>,
  ];
};

TablePrimaryKeys.propTypes = {
  columns: PropTypes.array.isRequired,
  primaryKeys: PropTypes.array.isRequired,
  onRemove: PropTypes.func.isRequired,
  onChange: PropTypes.func.isRequired,
};

export default TablePrimaryKeys;
