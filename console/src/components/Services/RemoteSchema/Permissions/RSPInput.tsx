import React, { useState } from 'react';
import { GraphQLInputField } from 'graphql';
import Pen from './Pen';
import { useDebouncedEffect } from '../../../../hooks/useDebounceEffect';

interface RSPInputProps {
  k: string;
  editMode: boolean;
  value: string | Record<string, any>;
  v: GraphQLInputField;
  setArgVal: (v: Record<string, unknown>) => void;
  setEditMode: (b: boolean) => void;
}
const RSPInput: React.FC<RSPInputProps> = ({
  k,
  editMode,
  value,
  setArgVal,
  v,
  setEditMode,
}) => {
  const [localValue, setLocalValue] = useState<string>(value as string);

  // TODO check more on the browser console error message
  useDebouncedEffect(
    () => {
      if (
        (v?.type?.inspect() === 'Int' || v?.type?.inspect() === 'Int!') &&
        localValue &&
        !Number.isNaN(Number(localValue))
      ) {
        console.log(Number(localValue), Number.isNaN(Number(localValue)));
        if (localValue === '0') return setArgVal({ [v?.name]: 0 });
        return setArgVal({ [v?.name]: Number(localValue) });
      }

      setArgVal({ [v?.name]: localValue });
    },
    500,
    [localValue]
  ); // ignore continues onChange events till the user finish typing
  // TODO support different input types :?
  return (
    <>
      <label htmlFor={k}> {k}:</label>
      {editMode ? (
        <>
          <input
            value={localValue}
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

export default React.memo(RSPInput);
