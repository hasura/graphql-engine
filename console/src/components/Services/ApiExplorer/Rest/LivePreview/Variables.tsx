import React, { ChangeEvent } from 'react';

import { VariableState } from './state';
import Input from './Input';
import PreviewTable from './PreviewTable';
import ToolTip from '../../../../Common/Tooltip/Tooltip';

import styles from '../RESTStyles.scss';

type VariableComponentProps = {
  updateVariableValue: (
    name: string
  ) => (e: ChangeEvent<HTMLInputElement>) => void;
  variablesState: VariableState[];
};

const requestVariablesHeadings = [
  { content: 'Name', className: styles.rest_preview_w20 },
  { content: 'Type' },
  { content: 'Value' },
];

const Variables: React.FC<VariableComponentProps> = ({
  variablesState,
  updateVariableValue,
}) => {
  if (!variablesState || !variablesState.length) {
    return (
      <div className={styles.rest_empty_container}>
        This query doesn&apos;t require any request variables
      </div>
    );
  }

  return (
    <PreviewTable headings={requestVariablesHeadings}>
      {variablesState.map(v => (
        <tr
          className={styles.rest_preview_table_row}
          key={`rest-var-${v.name}`}
        >
          <td className={styles.text_center}>
            <b>{v.name}</b>
          </td>
          <td>
            <b>
              {v.type || v.kind}
              {v.kind === 'NonNullType' && '!'}
            </b>
            {v.kind === 'Unsupported' && (
              <ToolTip
                message="Complex types are not supported"
                placement="bottom"
              />
            )}
          </td>
          <td>
            <Input
              value={v.value}
              placeholder="Value..."
              onChange={updateVariableValue(v.name)}
              type="text"
            />
          </td>
        </tr>
      ))}
    </PreviewTable>
  );
};

export default Variables;
