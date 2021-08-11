import React from 'react';

import Tooltip from '../../../Common/Tooltip/Tooltip';
import IndexFieldsEditor from './IndexFieldsEditor';
import { Table } from '../../../../dataSources/types';

import styles from './ModifyTable.scss';

type Props = {
  tableSchema: Table;
};

const tooltipMessage =
  'Indexes are used to increase query performance based on columns that are queried frequently';

const IndexFields: React.FC<Props> = props => (
  <>
    <h4 className={styles.subheading_text}>
      Indexes &nbsp; &nbsp;
      <Tooltip message={tooltipMessage} />
    </h4>
    <IndexFieldsEditor currentTableInfo={props.tableSchema} />
  </>
);

export default IndexFields;
