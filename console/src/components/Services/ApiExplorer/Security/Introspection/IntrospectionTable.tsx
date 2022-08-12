import React from 'react';
import {
  Table,
  TableForm,
  TableHeader,
  TableLegend,
  TableRow,
  TableSideBar,
} from '../../../../Common/Table';
import styles from '../Security.scss';
import { Legends } from '../utils';
import IntrospectionForm from './IntrospectionForm';

type Props = {
  rows: { roleName: string; instrospectionIsDisabled: boolean }[];
  onClick?: () => { roleName: string; instrospectionIsDisabled: boolean };
};

const IntrospectionTable: React.FC<Props> = ({ rows }) => {
  const headers = ['Role', 'Schema Introspection'];

  const keys = ['roleName', 'instrospectionIsDisabled'];
  return (
    <div className={styles.max_width_80}>
      <Table columnCount={keys.length} rowCount={rows.length + 2}>
        <TableHeader headers={headers} keys={keys} />

        <TableSideBar
          items={['admin', ...rows.map(item => item.roleName), '']}
          renderItem={item => item}
        />

        <TableRow
          index="admin"
          entries={['full access']}
          isSingleColumn
          readonly
          renderCol={({ data }) => data}
        />

        {rows.map(item => (
          <TableRow
            // starts index by 2, since we have two rows preceding this
            index={item.roleName}
            entries={[item.instrospectionIsDisabled]}
            isSingleColumn
            renderCol={({ data }) => (
              <div>
                <span>{data ? <Legends.Disabled /> : <Legends.Enabled />}</span>
              </div>
            )}
          />
        ))}

        <TableRow
          index=""
          entries={[{ roleName: '', instrospectionIsDisabled: true }]}
          isSingleColumn
          renderCol={() => (
            <>
              <Legends.Global />
            </>
          )}
        />

        <TableForm<{ roleName: string; instrospectionIsDisabled: boolean }>>
          {props => {
            return <IntrospectionForm {...props} />;
          }}
        </TableForm>
        <TableLegend>
          <ul className={styles.legend}>
            <li>
              <Legends.Enabled />: enabled
            </li>
            <li>
              <Legends.Disabled />: disabled
            </li>
            <li>
              <Legends.Global />: global setting
            </li>
          </ul>
        </TableLegend>
      </Table>
    </div>
  );
};

export default IntrospectionTable;
