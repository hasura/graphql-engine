import React, { useCallback, useState } from 'react';
import { FieldType, RSPTreeComponentProps, ExpandedItems } from './types';
import { Field } from './Field';
import { checkTypesRecursively } from './utils';

const Tree: React.FC<RSPTreeComponentProps> = ({
  list,
  setState,
  checkTypes,
}) => {
  // TODO add checkbox
  // TODO create and sync tree
  // TODO check actual gql schema structure and change, if required
  const [expandedItems, setExpandedItems] = useState<ExpandedItems>({});
  const onCheck = useCallback(
    ix => (e: React.FormEvent<HTMLInputElement>) => {
      const newList = [...list] as FieldType[];
      const target = e.target as HTMLInputElement;
      newList[ix] = { ...list[ix], checked: target.checked };
      console.log(newList[ix]);
      const field = newList[ix];
      checkTypes(field.checked as boolean, field.return as string);
      setState([...newList]);
    },
    [setState, list]
  );

  const setItem = useCallback(
    (ix: number) => (newState: FieldType) => {
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
          <Field
            i={i}
            setItem={setItem(ix)}
            key={i.name}
            onExpand={toggleExpand(ix)}
          />
          {i.children && expandedItems[ix] && (
            <MemoizedTree
              list={i.children}
              setState={setValue(ix)}
              checkTypes={checkTypes}
            />
          )}
        </li>
      ))}
    </ul>
  );
};

const MemoizedTree = React.memo(Tree);
export default MemoizedTree;
