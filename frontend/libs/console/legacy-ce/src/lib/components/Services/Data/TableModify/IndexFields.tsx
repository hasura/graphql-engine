import React from 'react';
import IndexFieldsEditor from './IndexFieldsEditor';
import { Table } from '../../../../dataSources/types';

type Props = {
  tableSchema: Table;
};

const IndexFields: React.FC<Props> = props => (
  <IndexFieldsEditor currentTableInfo={props.tableSchema} />
);

export default IndexFields;
