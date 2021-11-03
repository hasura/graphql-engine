import React from 'react';
import Dropdown from '../../../../Common/Dropdown/Dropdown';
import Button from '../../../../Common/Button/Button';
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
  onSelect: any;
  action: ColumnAction | null;
  dispatch: Dispatch | null;
  postgresVersion: string | null;
}

const FrequentlyUsedColumnSelector = ({
  onSelect,
  action = null,
  dispatch = null,
  postgresVersion,
}: FrequentlyUsedColumnSelectorProps) => {
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
      <Button color="white" size="xs">
        + Frequently used columns
      </Button>
    </Dropdown>
  );
};

export default FrequentlyUsedColumnSelector;
