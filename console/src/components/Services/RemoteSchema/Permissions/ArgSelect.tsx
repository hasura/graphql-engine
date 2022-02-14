import React, { useRef, useEffect, useState, ReactText } from 'react';
import merge from 'lodash.merge';
import { GraphQLInputField } from 'graphql';
import { getChildArguments } from './utils';
import RSPInput from './RSPInput';
import { ArgTreeType } from './types';
import styles from '../../../Common/Permissions/PermissionStyles.scss';

interface ArgSelectProps {
  valueField: GraphQLInputField;
  keyName: string;
  value?: ArgTreeType | ReactText;
  level: number;
  setArg: (e: Record<string, unknown>) => void;
  isInputObjectType?: boolean;
  isFirstLevelInputObjPreset?: boolean;
}

export const ArgSelect: React.FC<ArgSelectProps> = ({
  keyName: k,
  valueField: v,
  value,
  level,
  setArg = e => console.log(e),
  isInputObjectType,
  isFirstLevelInputObjPreset,
}) => {
  const [expanded, setExpanded] = useState(false);
  const autoExpanded = useRef(false);
  const [editMode, setEditMode] = useState<boolean>(
    Boolean(
      value &&
        ((typeof value === 'string' && value.length > 0) ||
          typeof value === 'number')
    )
  );
  const prevState = useRef<Record<string, any>>();
  useEffect(() => {
    if (value && typeof value === 'string' && value.length > 0 && !editMode) {
      // show value instead of pen icon, if the value is defined in the prop
      setEditMode(true);
    }
  }, [value, editMode]);

  useEffect(() => {
    // auto expand args when there is prefilled values
    // happens only first time when the node is created
    if (value && k && !expanded && !autoExpanded.current) {
      setExpanded(true);
      autoExpanded.current = true;
    }
  }, [value, k, expanded]);

  // for input object types having children, hide first level args
  const hideInputArgName = isInputObjectType && level === 0;
  const { children } = getChildArguments(v as GraphQLInputField);
  if (hideInputArgName && children) {
    if (!expanded) setExpanded(true);
  }

  const setArgVal = (val: Record<string, any>) => {
    const prevVal = prevState.current;
    if (prevVal) {
      const newState = merge(prevVal, val);
      setArg(newState);
      prevState.current = newState;
    } else {
      setArg(val);
      prevState.current = val;
    }
  };

  const toggleExpandMode = () => setExpanded(b => !b);

  if (children) {
    return (
      <>
        {!hideInputArgName ? (
          <>
            <button onClick={toggleExpandMode} style={{ marginLeft: '-1em' }}>
              {expanded ? '-' : '+'}
            </button>
            {!expanded && (
              <label
                className={`${styles.argSelect} ${styles.fw_medium}`}
                htmlFor={k}
              >
                {k}:
              </label>
            )}
            {expanded && (
              <label
                className={`${styles.argSelect} ${styles.fw_large}`}
                htmlFor={k}
              >
                {k}:
              </label>
            )}
          </>
        ) : null}
        {hideInputArgName ? (
          expanded &&
          Object.values(children).map(i => {
            if (typeof value === 'string') return undefined;
            const childVal =
              value && typeof value === 'object' ? value[i?.name] : undefined;
            return (
              <li key={i.name}>
                <ArgSelect
                  keyName={i.name}
                  setArg={val => setArgVal({ [k]: val })}
                  valueField={i}
                  value={childVal}
                  level={level + 1}
                />
              </li>
            );
          })
        ) : (
          <ul>
            {expanded &&
              Object.values(children).map(i => {
                if (typeof value === 'string') return undefined;
                const childVal =
                  value && typeof value === 'object'
                    ? value[i?.name]
                    : undefined;
                return (
                  <li key={i.name}>
                    <ArgSelect
                      keyName={i.name}
                      setArg={val => setArgVal({ [k]: val })}
                      valueField={i}
                      value={childVal}
                      level={level + 1}
                    />
                  </li>
                );
              })}
          </ul>
        )}
      </>
    );
  }
  if (isFirstLevelInputObjPreset) {
    return (
      <RSPInput
        v={v as GraphQLInputField}
        k={k}
        editMode
        setArgVal={setArgVal}
        value={value}
        setEditMode={() => {}}
        isFirstLevelInputObjPreset
      />
    );
  }
  return (
    <li>
      <RSPInput
        v={v as GraphQLInputField}
        k={k}
        editMode={editMode}
        setArgVal={setArgVal}
        value={value}
        setEditMode={setEditMode}
      />
    </li>
  );
};
