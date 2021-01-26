import React, { useState, useEffect } from 'react';
import { GraphQLInputField } from 'graphql';
import Pen from './Pen';
import { useDebouncedEffect } from '../../../../hooks/useDebounceEffect';
import { isNumberString } from '../../../Common/utils/jsUtils';

interface RSPInputProps {
  k: string;
  editMode: boolean;
  value: string | Record<string, any>;
  v: GraphQLInputField;
  setArgVal: (v: Record<string, unknown>) => void;
  setEditMode: (b: boolean) => void;
}
const RSPInputComponent: React.FC<RSPInputProps> = ({
  k,
  editMode,
  value = '',
  setArgVal,
  v,
  setEditMode,
}) => {
  const [localValue, setLocalValue] = useState<string>(
    typeof value === 'object' ? '' : value
  );

  const inputRef = React.useRef<HTMLInputElement>(null);

  // focus the input element; onClick of Pen Icon
  useEffect(() => {
    if (editMode && inputRef && inputRef.current) inputRef.current.focus();
  }, [editMode]);

  useDebouncedEffect(
    () => {
      if (
        (v?.type?.inspect() === 'Int' || v?.type?.inspect() === 'Int!') &&
        localValue &&
        isNumberString(localValue)
      ) {
        if (localValue === '0') return setArgVal({ [v?.name]: 0 });
        return setArgVal({ [v?.name]: Number(localValue) });
      }

      setArgVal({ [v?.name]: localValue });
    },
    500,
    [localValue]
  );
  return (
    <>
      <label htmlFor={k}> {k}:</label>
      {editMode ? (
        <>
          <input
            value={localValue}
            ref={inputRef}
            style={{
              border: 0,
              borderBottom: '2px dotted black',
              borderRadius: 0,
            }}
            onChange={e => setLocalValue(e.target.value)}
          />
        </>
      ) : (
        <button onClick={() => setEditMode(true)}>
          <Pen />
        </button>
      )}
    </>
  );
};

const RSPInput = React.memo(RSPInputComponent);
export default RSPInput;
