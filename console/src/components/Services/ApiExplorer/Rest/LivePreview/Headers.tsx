import React, { ChangeEvent } from 'react';

import { HeaderState } from './state';
import Input from './Input';
import PreviewTable from './PreviewTable';

import styles from '../RESTStyles.scss';

type UpdateHeaderValues = (
  index: number
) => (e: ChangeEvent<HTMLInputElement>) => void;

type HeaderComponentProps = {
  updateKeyText: UpdateHeaderValues;
  updateValueText: UpdateHeaderValues;
  toggleActiveState: UpdateHeaderValues;
  onClickRemove: (index: number) => () => void;
  headerState: HeaderState[];
};

const requestHeadersHeadings = [
  {
    content: '',
    className: styles.rest_preview_table_sm_width,
  },
  { content: 'Key' },
  { content: 'Value' },
  { content: '' },
];

const Headers: React.FC<HeaderComponentProps> = ({
  headerState,
  updateKeyText,
  updateValueText,
  toggleActiveState,
  onClickRemove,
}) => {
  if (!headerState || !headerState.length) {
    return (
      <div className={styles.rest_empty_container}>
        Click on the &apos;Add Header&apos; to add some Request Headers
      </div>
    );
  }

  return (
    <PreviewTable headings={requestHeadersHeadings}>
      {headerState.map(header => (
        <tr
          className={styles.rest_preview_table_row}
          key={`rest-header-${header.index}`}
        >
          <td className={styles.text_center}>
            <input
              type="checkbox"
              value={header.key}
              onChange={toggleActiveState(header.index)}
              checked={header.isActive}
              className="legacy-input-fix"
            />
          </td>
          <td>
            <Input
              value={header.key}
              onChange={updateKeyText(header.index)}
              placeholder="Key..."
            />
          </td>
          <td>
            {header.key === 'admin-secret' ? (
              <Input
                value={header.value}
                onChange={updateValueText(header.index)}
                placeholder="Value..."
                type="password"
              />
            ) : (
              <Input
                value={header.value}
                onChange={updateValueText(header.index)}
                placeholder="Value..."
                type="text"
              />
            )}
          </td>
          <td>
            <div
              className={styles.rest_preview_clear_btn}
              onClick={onClickRemove(header.index)}
            >
              <i className="fa fa-times-circle" />
            </div>
          </td>
        </tr>
      ))}
    </PreviewTable>
  );
};

export default Headers;
