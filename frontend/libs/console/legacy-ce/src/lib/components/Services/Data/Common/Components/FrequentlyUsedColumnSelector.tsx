import React from 'react';
import { Button } from '../../../../../new-components/Button';
import Dropdown from '../../../../Common/Dropdown/Dropdown';
import { Dispatch } from '../../../../../types';
import {
  FrequentlyUsedColumn,
  ColumnAction,
} from '../../../../../dataSources/types';
import { frequentlyUsedColumns } from '../../../../../dataSources/services/postgresql/sqlUtils';

const getFreqUsedColDisplayInfo = (c: FrequentlyUsedColumn) => {
  const title = c.name;

  const typeText = `${c.typeText}; `;
  const defaultText =
    c.defaultText || c.default
      ? `default: ${c.defaultText || c.default}; `
      : '';
  const pkText = c.primary ? 'primary key; ' : '';

  const subTitle = typeText + defaultText + pkText;

  return {
    title,
    subTitle,
  };
};

interface FrequentlyUsedColumnSelectorProps {
  onSelect: (column: FrequentlyUsedColumn) => any;
  postgresVersion: string | null;
  action?: ColumnAction | null;
  dispatch?: Dispatch | null;
}

const FrequentlyUsedColumnSelector: React.VFC<
  FrequentlyUsedColumnSelectorProps
> = ({ onSelect, postgresVersion, action = null, dispatch = null }) => {
  const frequentlyUsedColumnsOptions = frequentlyUsedColumns
    .filter(fuc => !action || fuc.validFor.includes(action))
    .filter(col =>
      postgresVersion && col.minPGVersion
        ? parseFloat(postgresVersion) >= col.minPGVersion
        : true
    )
    .map(fuc => {
      const { title, subTitle } = getFreqUsedColDisplayInfo(fuc);
      return {
        content: (
          <div>
            <div>
              <b>{title}</b>
            </div>
            <div>{subTitle}</div>
          </div>
        ),
        onClick: () => (dispatch ? dispatch(onSelect(fuc)) : onSelect(fuc)),
      };
    });

  return (
    <Dropdown
      testId="frequently-used-columns"
      options={frequentlyUsedColumnsOptions}
      data-test="frequently-used-columns"
      position="bottom"
      key="frequently-used-columns"
      keyPrefix="frequently-used-columns"
    >
      {({ onClick }) => (
        <Button size="sm" onClick={onClick}>
          + Frequently used columns
        </Button>
      )}
    </Dropdown>
  );
};

export default FrequentlyUsedColumnSelector;
