import React, { useState, useEffect, ReactText } from 'react';
import { GraphQLEnumType, GraphQLInputField, GraphQLScalarType } from 'graphql';
import Pen from './Pen';
import { useDebouncedEffect } from '../../../../hooks/useDebounceEffect';
import { isNumberString } from '../../../Common/utils/jsUtils';
import { ArgTreeType } from './types';
import styles from '../../../Common/Permissions/PermissionStyles.scss';

interface RSPInputProps {
  k: string;
  editMode: boolean;
  value?: ArgTreeType | ReactText;
  v: GraphQLInputField;
  setArgVal: (v: Record<string, unknown>) => void;
  setEditMode: (b: boolean) => void;
  isFirstLevelInputObjPreset?: boolean;
}
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

  const [localValue, setLocalValue] = useState<ReactText>(
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
            }}
            onChange={e => setLocalValue(e.target.value)}
          />
          {isSessionvar() && (
            <button
              value="X-Hasura-User-Id"
              onClick={toggleSessionVariable}
              className={styles.sessionVarButton}
            >
              [X-Hasura-User-Id]
            </button>
          )}
        </>
      ) : (
        <button data-test={`pen-${k}`} onClick={() => setEditMode(true)}>
          <Pen />
        </button>
      )}
    </>
  );
};

const RSPInput = React.memo(RSPInputComponent);
export default RSPInput;
