import React from 'react';
import styles from '../../../../Common/Common.scss';

const RootFieldEditor = ({
  aliases,
  disabled,
  selectOnChange,
  selectByPkOnChange,
  selectAggOnChange,
  insertOnChange,
  updateOnChange,
  deleteOnChange,
}) => {
  const {
    select,
    select_by_pk: selectByPk,
    select_aggregate: selectAgg,
    insert,
    update,
    delete: _delete,
  } = aliases;

  const [queryExpanded, setQueryExpanded] = React.useState(true);
  const [mutationExpanded, setMutationExpanded] = React.useState(true);

  const getRow = (label, value, onChange) => (
    <div
      className={`${styles.display_flex} row ${styles.add_mar_bottom_small}`}
    >
      <div className={`${styles.add_mar_right} col-md-3`}>{label}</div>
      <div className={'col-md-3'}>
        <input
          type="text"
          value={value}
          placeholder={disabled ? 'unset' : ''}
          className="form-control"
          onChange={onChange}
          disabled={disabled}
        />
      </div>
    </div>
  );

  const getQuerySection = () => {
    const toggleQueryExpanded = () => setQueryExpanded(!queryExpanded);
    const chevronClassname = `fa fa-chevron-${
      queryExpanded ? 'down' : 'right'
    }`;
    return (
      <div className={`${styles.add_mar_bottom_mid}`}>
        <div
          className={`${styles.add_mar_bottom_small} ${styles.display_flex} ${
            styles.cursorPointer
          }`}
          onClick={toggleQueryExpanded}
        >
          <i className={`${chevronClassname} ${styles.add_mar_right}`} />
          <b> Query and Subscription </b>
        </div>
        {queryExpanded && (
          <div className={`${styles.add_pad_left} ${styles.add_pad_right}`}>
            {getRow('Select', select, selectOnChange)}
            {getRow('Select by PK', selectByPk, selectByPkOnChange)}
            {getRow('Select Aggregate', selectAgg, selectAggOnChange)}
          </div>
        )}
      </div>
    );
  };

  const getMutationSection = () => {
    const toggleMutationExpanded = () => setMutationExpanded(!mutationExpanded);
    const chevronClassname = `fa fa-chevron-${
      mutationExpanded ? 'down' : 'right'
    }`;
    return (
      <div className={`${styles.add_mar_bottom_mid}`}>
        <div
          className={`${styles.add_mar_bottom_small} ${styles.display_flex} ${
            styles.cursorPointer
          }`}
          onClick={toggleMutationExpanded}
        >
          <i className={`${chevronClassname} ${styles.add_mar_right}`} />
          <b> Mutation </b>
        </div>
        {mutationExpanded && (
          <div className={`${styles.add_pad_left} ${styles.add_pad_right}`}>
            {getRow('Insert', insert, insertOnChange)}
            {getRow('Update', update, updateOnChange)}
            {getRow('Delete', _delete, deleteOnChange)}
          </div>
        )}
      </div>
    );
  };

  return (
    <div>
      <div className={styles.add_mar_bottom_mid}>
        {getQuerySection()}
        {getMutationSection()}
      </div>
    </div>
  );
};

export default RootFieldEditor;
