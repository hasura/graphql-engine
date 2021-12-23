import React from 'react';

import { OrderBy } from '../utils/v1QueryUtils';
import { useFilterQuery, TriggerOperation } from './state';
import { Filter, FilterRenderProp } from './types';
import { Nullable } from '../utils/tsUtils';
import styles from './FilterQuery.scss';
import { BaseTable } from '../../../dataSources/types';
import { generateTableDef } from '../../../dataSources';
import { Dispatch } from '../../../types';
import { EventKind } from '../../Services/Events/types';

interface Props {
  table: BaseTable;
  relationships: Nullable<string[]>;
  render: FilterRenderProp;
  presets: {
    filters: Filter[];
    sorts: OrderBy[];
  };
  dispatch: Dispatch;
  triggerOp: TriggerOperation;
  triggerType: EventKind;
  triggerName?: string;
  currentSource?: string; // mainly needed by data triggers
}

/*
 * Where clause and sorts builder
 * Accepts a render prop to render the results of filter/sort query
 */

const FilterQuery: React.FC<Props> = props => {
  const {
    table,
    dispatch,
    presets,
    render,
    relationships,
    triggerName,
    currentSource,
    triggerOp,
    triggerType,
  } = props;

  const { rows, count, runQuery, state, setState } = useFilterQuery(
    generateTableDef(table.table_name, table.table_schema),
    dispatch,
    presets,
    relationships,
    triggerOp,
    triggerType,
    triggerName,
    currentSource
  );

  return (
    <div className={styles.add_mar_top}>
      {render(rows, count, state, setState, runQuery)}
    </div>
  );
};

export default FilterQuery;
