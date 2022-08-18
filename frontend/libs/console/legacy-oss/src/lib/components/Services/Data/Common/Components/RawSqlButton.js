import React from 'react';
import _push from '../../push';
import { SET_SQL } from '../../RawSQL/Actions';
import Button from '../../../../Common/Button/Button';

const RawSqlButton = props => {
  const { dataTestId, sql, customStyles, dispatch, children } = props;

  const handleClick = e => {
    e.preventDefault();

    dispatch(_push(`/data/sql`));

    dispatch({
      type: SET_SQL,
      data: sql,
    });
  };

  return (
    <Button
      data-test={dataTestId}
      className={`${customStyles} btn btn-xs btn-default`}
      onClick={handleClick}
    >
      {children}
    </Button>
  );
};

export default RawSqlButton;
