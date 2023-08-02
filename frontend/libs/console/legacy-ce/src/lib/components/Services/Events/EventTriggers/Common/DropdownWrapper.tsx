import React, { useEffect, useState } from 'react';
import { useDebouncedEffect } from '../../../../../hooks/useDebounceEffect';
import DropdownButton from '../../../../Common/DropdownButton/DropdownButton';

type DropdownWrapperProps = {
  title: string;
  dropdownOptions: {
    display_text: string;
    value: string;
  }[];
  dataKey: string;
  dataIndex?: string;
  onButtonChange: (e: React.MouseEvent<unknown>) => void;
  onHandlerValChange: (v: string) => void;
  value?: string;
  handlerVal: string;
  required: boolean;
  id?: string;
  testId?: string;
  disabled?: boolean;
  bsClass?: string;
  inputPlaceHolder: string;
  inputStyle?: Record<string, string>;
};

const DropdownWrapper: React.FC<DropdownWrapperProps> = props => {
  const { onHandlerValChange, handlerVal } = props;
  const [localValue, setLocalValue] = useState<string>(handlerVal);

  useEffect(() => {
    setLocalValue(handlerVal);
  }, [handlerVal]);

  useDebouncedEffect(
    () => {
      onHandlerValChange(localValue);
    },
    1000,
    [localValue]
  );

  const onLocalValChangeHandler = (e: React.ChangeEvent<HTMLInputElement>) => {
    setLocalValue(e.target.value);
  };

  return (
    <DropdownButton
      inputVal={localValue}
      onInputChange={onLocalValChangeHandler}
      {...props}
    />
  );
};

export default DropdownWrapper;
