import React, { useRef, useEffect, useState } from 'react'
import merge from 'lodash.merge';
import { getChildArgument } from './utils';
import RSPInput from './RSPInput';

interface ArgSelectProps {
  v: Record<string, any>|unknown;
  k: string;
  value: string | Record<string, any>;
  level: number;
  setArg: (e: Record<string, unknown>) => void;
}

export const ArgSelect: React.FC<ArgSelectProps> = ({ k, v, value, level, setArg = e => console.log(e) }) => {
  const [expanded, setExpanded] = useState(false);
  const autoExpanded = useRef(false)
  const [editMode, setEditMode] = useState(
    value && (typeof value === 'string' && value.length > 0 || typeof value === 'number')
  );
  const prevState = useRef();
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
      setExpanded(true)
      autoExpanded.current = true
    }
  }, [value, k, expanded])

  const { children } = getChildArgument(v);

  const setArgVal = (val) => {
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
          Object.values(children).map((i) => {
            const childVal = value ? value[i?.name] : undefined;
            return (
              <li>
                <ArgSelect
                  {...{
                    k: i.name,
                    setArg: val => setArgVal({ [k]: val }),
                    v: i,
                    value: childVal,
                    level: level + 1,
                  }}
                />
              </li>
            );
          })}
      </ul>
    );
  }
  return (
    <li>
      <RSPInput {...{ k, editMode, value, setArgVal, v, setEditMode }} />
    </li>
  );
};
