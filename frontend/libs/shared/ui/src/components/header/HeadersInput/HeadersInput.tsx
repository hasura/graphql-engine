import React from 'react';
import { Input, Select } from '../../Form';
import { IconButton } from '../../Button';
import { addPlaceholderHeader } from './utils';
import { Flex } from '@radix-ui/themes';
import { FaTrash } from 'react-icons/fa6';
import { ClientHeader } from '@hasura/shared/types';

export interface HeadersInputProps extends React.ComponentProps<'div'> {
  headers: ClientHeader[];
  disabled?: boolean;
  setHeaders: (h: ClientHeader[]) => void;
}

const valueTypeOptions = [
  {
    label: 'Value',
    value: 'value',
  },
  {
    label: 'From env var',
    value: 'env',
  },
];

export const HeadersInput: React.FC<HeadersInputProps> = ({
  headers,
  setHeaders,
  disabled = false,
}) => {
  return (
    <Flex direction="column" gap="2">
      {headers.map(({ name, value, type }, i) => {
        const setHeaderType = (newType: string) => {
          const newHeaders = headers.map((header, index) =>
            i === index
              ? {
                  ...header,
                  type: newType as 'value' | 'env',
                }
              : header,
          );
          addPlaceholderHeader(newHeaders);
          setHeaders(newHeaders);
        };

        const setHeaderKey = (e: React.ChangeEvent<HTMLInputElement>) => {
          const newHeaders = JSON.parse(JSON.stringify(headers));
          newHeaders[i].name = e.target.value;
          addPlaceholderHeader(newHeaders);
          setHeaders(newHeaders);
        };

        const setHeaderValue = (e: React.ChangeEvent<HTMLInputElement>) => {
          const newHeaders = JSON.parse(JSON.stringify(headers));
          newHeaders[i].value = e.target.value;
          addPlaceholderHeader(newHeaders);
          setHeaders(newHeaders);
        };

        const removeHeader = () => {
          const newHeaders = JSON.parse(JSON.stringify(headers));
          setHeaders([...newHeaders.slice(0, i), ...newHeaders.slice(i + 1)]);
        };

        return (
          <Flex gap="2" align="center" key={i.toString()}>
            <div className="w-full sm:w-4/12">
              <Input
                value={name}
                onChange={setHeaderKey}
                placeholder="key"
                disabled={disabled}
                full
              />
            </div>
            <div className="w-full sm:w-40">
              <Select
                full
                disabled={disabled}
                data-test={`header-value-${i}-dropdown-button`}
                value={type}
                onChange={setHeaderType}
                options={valueTypeOptions}
              />
            </div>
            <div className="w-full sm:w-4/12">
              <Input
                type="text"
                required={false}
                onChange={setHeaderValue}
                disabled={disabled}
                value={value || ''}
                placeholder={type === 'env' ? 'HEADER_FROM_ENV' : 'value'}
                data-test={`header-value-${i}-input`}
                id={`header-value-${i}`}
                full
              />
            </div>
            {i < headers.length - 1 ? (
              <IconButton
                variant="ghost"
                onClick={removeHeader}
                mode="destructive"
                radius="full"
              >
                <FaTrash />
              </IconButton>
            ) : null}
          </Flex>
        );
      })}
    </Flex>
  );
};
