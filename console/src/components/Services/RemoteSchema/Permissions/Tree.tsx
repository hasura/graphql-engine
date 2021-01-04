import React, { useCallback, useState } from 'react';
import { FieldType, RSPTreeComponentProps, ExpandedItems } from './types';
import { Field } from './Field';

const Tree: React.FC<RSPTreeComponentProps> = ({ list, setState }) => {
  // TODO add checkbox
  // TODO create and sync tree
  // TODO check actual gql schema structure and change, if required
  const [expandedItems, setExpandedItems] = useState<ExpandedItems>({});
  const onCheck = useCallback(
    ix => (e: React.FormEvent<HTMLInputElement>) => {
      const newList = [...list] as FieldType[];
      newList[ix] = { ...list[ix], checked: e.target.checked };
      setState([...newList]);
    },
    [setState, list]
  );

  const setItem = useCallback(
    ix => newState => {
      const newList = [...list];
      newList[ix] = { ...newState };
      setState([...newList]);
    },
    [setState, list]
  );
  const setValue = useCallback(
    ix => (newState: FieldType[]) => {
      const newList = [...list];
      newList[ix] = { ...list[ix], children: [...newState] };
      setState([...newList]);
    },
    [setState, list]
  );
  const toggleExpand = (ix: number) => () => {
    setExpandedItems(oldExpandedItems => {
      const newState = !oldExpandedItems[ix];
      const newExpandeditems = { ...oldExpandedItems, [ix]: newState };
      return newExpandeditems;
    });
  };
  return (
    <ul>
      {list.map((i: FieldType, ix) => (
        <li key={i.name}>
          {i.checked !== undefined && (
            <input
              type="checkbox"
              id={i.name}
              name={i.name}
              checked={i.checked}
              onChange={onCheck(ix)}
            />
          )}
          {i.children && (
            <button onClick={toggleExpand(ix)}>
              {expandedItems[ix] ? '-' : '+'}
            </button>
          )}
          <Field i={i} setItem={setItem(ix)} key={i.name} />
          {i.children && expandedItems[ix] && (
            <MemoizedTree list={i.children} setState={setValue(ix)} />
          )}
        </li>
      ))}
    </ul>
  );
};

const MemoizedTree = React.memo(Tree);
export default MemoizedTree;
