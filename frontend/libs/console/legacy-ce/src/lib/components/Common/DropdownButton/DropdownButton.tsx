import React from 'react';
import InputGroup from 'react-bootstrap/lib/InputGroup';
import * as DropdownMenu from '@radix-ui/react-dropdown-menu';

type DropDownButtonProps = {
  title: string;
  dropdownOptions: {
    display_text: string;
    value: string;
  }[];
  dataKey: string;
  dataIndex?: string;
  onButtonChange: (e: React.MouseEvent<unknown>) => void;
  onInputChange: (e: React.ChangeEvent<HTMLInputElement>) => void;
  value?: string;
  inputVal: string;
  required: boolean;
  id?: string;
  testId?: string;
  disabled?: boolean;
  bsClass?: string;
  inputPlaceHolder: string;
  inputStyle?: Record<string, string>;
};

/*
Hello traveler of the old code.
This was originally made using react boostrap, but for some reason, react boostrap
stopped having the dropdown working. This was migrated 1:1 using radix ui
in order to have the old code still work.

This is not to be used with newer code !!!!!!!
 */

// because the previous code was doing some wierd things with non existing attributes
/* eslint-disable @typescript-eslint/ban-ts-comment */

const DDButton: React.FC<DropDownButtonProps> = props => {
  const {
    title,
    dropdownOptions,
    value,
    required,
    onInputChange,
    onButtonChange,
    dataKey,
    dataIndex,
    bsClass,
    disabled,
    inputVal,
    inputPlaceHolder,
    id,
    testId,
  } = props;
  return (
    <InputGroup className={bsClass}>
      <span className="dropdown input-group-btn">
        <DropdownMenu.Root>
          <DropdownMenu.Trigger asChild>
            <button
              className="dropdown-toggle btn btn-default"
              disabled={disabled}
              id={id || ''}
              data-test={`${testId}-dropdown-button`}
            >
              {value || title}
              <span className="caret !ml-1"></span>
            </button>
          </DropdownMenu.Trigger>
          <DropdownMenu.Portal>
            <DropdownMenu.Content
              className="flex flex-col items-start bg-white border py-1"
              style={{
                border: '1px solid rgba(0, 0, 0, 0.15)',
                borderRadius: '4px',
                boxShadow: '0 6px 12px rgb(0 0 0 / 18%);',
                padding: '5px 0',
                margin: '2px 0 0',
                zIndex: 100
              }}
              align="start"
            >
              {dropdownOptions.map((d, i) => (
                <DropdownMenu.Item asChild>
                  <button
                    data-index-id={dataIndex}
                    /*
                    // @ts-ignore */
                    value={d.value}
                    onClick={onButtonChange}
                    /*
                    // @ts-ignore */
                    eventKey={i + 1}
                    key={i}
                    data-test={`${testId}-dropdown-item-${i + 1}`}
                    style={{
                      padding: '3px 20px',
                      color: '#333333',
                      lineHeight: 1.42857,
                      fontWeight: 400,
                      fontSize: '14px',
                    }}
                    className="whitespace-nowrap focus-visible:bg-[#f5f5f5] hover:bg-[#f5f5f5] w-full text-left"
                  >
                    {d.display_text}
                  </button>
                </DropdownMenu.Item>
              ))}
            </DropdownMenu.Content>
          </DropdownMenu.Portal>
        </DropdownMenu.Root>
      </span>
      <input
        style={props.inputStyle}
        type="text"
        data-key={dataKey}
        data-index-id={dataIndex}
        className="form-control"
        required={required}
        onChange={onInputChange}
        disabled={disabled}
        value={inputVal || ''}
        placeholder={inputPlaceHolder}
        data-test={`${testId}-input`}
        name={`${dataKey}[${dataIndex}]`}
      />
    </InputGroup>
  );
};

export default DDButton;
