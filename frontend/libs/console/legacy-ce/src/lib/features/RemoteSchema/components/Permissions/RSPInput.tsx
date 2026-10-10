import React, { useState, useEffect } from 'react';
import {
  GraphQLEnumType,
  GraphQLInputField,
  GraphQLScalarType,
  isNonNullType,
  isScalarType,
} from 'graphql';
import Pen from './Pen';
import { useDebouncedEffect } from '@hasura/shared/hooks';
import { isNumberString } from '@hasura/shared/utils';
import { ArgTreeType } from './types';
import { FaTimes } from 'react-icons/fa';

interface RSPInputProps {
  k: string;
  editMode: boolean;
  value?: ArgTreeType | React.ReactNode;
  v: GraphQLInputField;
  setArgVal: (v: Record<string, unknown>) => void;
  setEditMode: (b: boolean) => void;
  isFirstLevelInputObjPreset?: boolean;
}

type EffectArg = {
  v: RSPInputProps['v'];
  localValue: React.ReactNode;
  setArgVal: RSPInputProps['setArgVal'];
};

export const rspInputEffect = ({ v, localValue, setArgVal }: EffectArg) => {
  if (!v) {
    return;
  }

  const isNumberType = (t: unknown) =>
    isScalarType(t) && (t.name === 'Int' || t.name === 'Int!');

  if (
    (isNonNullType(v.type)
      ? isNumberType(v.type.ofType)
      : isScalarType(v.type)) &&
    localValue &&
    isNumberString(localValue)
  ) {
    if (localValue === '0') {
      setArgVal({ [v?.name]: 0 });
      return;
    }
    setArgVal({ [v?.name]: Number(localValue) });
    return;
  }

  setArgVal({ [v?.name]: localValue });
};

const RSPInputComponent: React.FC<RSPInputProps> = ({
  k,
  editMode,
  value = '',
  setArgVal,
  v,
  setEditMode,
  isFirstLevelInputObjPreset,
}) => {
  const isSessionvar = () => {
    if (
      v.type instanceof GraphQLScalarType ||
      v.type instanceof GraphQLEnumType
    )
      return true;
    return false;
  };

  const [localValue, setLocalValue] = useState<string | number>(
    isNumberString(value) ? value : '',
  );

  const inputRef = React.useRef<HTMLInputElement>(null);

  // focus the input element; onClick of Pen Icon
  useEffect(() => {
    if (editMode && inputRef && inputRef.current) inputRef.current.focus();
  }, [editMode]);

  useDebouncedEffect(() => rspInputEffect({ v, localValue, setArgVal }), 500, [
    localValue,
  ]);

  const toggleSessionVariable = (e: React.MouseEvent) => {
    const input = e.target as HTMLButtonElement;
    setLocalValue(input.value);
  };

  return (
    <>
      {!isFirstLevelInputObjPreset ? <label htmlFor={k}> {k}:</label> : null}
      {editMode ? (
        <>
          <input
            value={localValue}
            ref={inputRef}
            data-test={`input-${k}`}
            style={{
              border: 0,
              borderBottom: '2px dotted black',
              borderRadius: 0,
              marginLeft: '8px',
            }}
            onChange={(e) => setLocalValue(e.target.value)}
          />
          {isSessionvar() && (
            <button
              value="X-Hasura-User-Id"
              onClick={toggleSessionVariable}
              className="text-green-600 cursor-pointer text-xs font-bold ml-2"
            >
              [X-Hasura-User-Id]
            </button>
          )}
          <button
            className="ml-2"
            data-test={`close-${k}`}
            onClick={() => {
              setLocalValue('');
              setEditMode(false);
            }}
          >
            <FaTimes />
          </button>
        </>
      ) : (
        <button
          className="ml-2"
          data-test={`pen-${k}`}
          onClick={() => setEditMode(true)}
        >
          <Pen />
        </button>
      )}
    </>
  );
};

const RSPInput = React.memo(RSPInputComponent);
export default RSPInput;
