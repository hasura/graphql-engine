import React, { ChangeEvent } from 'react';
import { FaTimesCircle } from 'react-icons/fa';

import { HeaderState } from './state';
import Input from './Input';
import PreviewTable from './PreviewTable';

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
    className: 'w-2/12',
  },
  { content: 'Key', className: 'p-md' },
  { content: 'Value', className: 'p-md' },
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
      <div className="w-full pt-md flex justify-center items-center font-lg font-bold">
        Click on the &apos;Add Header&apos; to add some Request Headers
      </div>
    );
  }

  return (
    <PreviewTable headings={requestHeadersHeadings}>
      {headerState.map(header => (
        <tr
          className="p-md border-b border-gray-300 mb-md"
          key={`rest-header-${header.index}`}
        >
          <td className="p-md">
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
              className="ml-xs cursor-pointer"
              onClick={onClickRemove(header.index)}
            >
              <FaTimesCircle />
            </div>
          </td>
        </tr>
      ))}
    </PreviewTable>
  );
};

export default Headers;
