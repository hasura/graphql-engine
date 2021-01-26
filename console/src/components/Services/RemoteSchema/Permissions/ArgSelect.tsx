import React, { useRef, useEffect, useState } from 'react';
import merge from 'lodash.merge';
import { GraphQLInputField } from 'graphql';
import { getChildArguments } from './utils';
import RSPInput from './RSPInput';

interface ArgSelectProps {
  valueField: GraphQLInputField;
  keyName: string;
  value: string | Record<string, any>;
  level: number;
  setArg: (e: Record<string, unknown>) => void;
}

export const ArgSelect: React.FC<ArgSelectProps> = ({
  keyName: k,
  valueField: v,
  value,
  level,
  setArg = e => console.log(e),
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

  const { children } = getChildArguments(v as GraphQLInputField);

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
      <ul>
        <button onClick={toggleExpandMode} style={{ marginLeft: '-1em' }}>
          {expanded ? '-' : '+'}
        </button>
        <label style={{ cursor: 'pointer' }} htmlFor={k}>
          {k}:
        </label>
        {expanded &&
          Object.values(children).map(i => {
            if (typeof value === 'string') return undefined;
            const childVal = value ? value[i?.name] : undefined;
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
