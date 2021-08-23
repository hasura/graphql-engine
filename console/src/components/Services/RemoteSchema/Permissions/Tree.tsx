import React, { useCallback, useEffect, useRef, useState } from 'react';
import { FieldType, ExpandedItems, PermissionEdit } from './types';
import { Field } from './Field';
import styles from '../../../Common/Permissions/PermissionStyles.scss';
import { addDepFields, getExpandedItems } from './utils';

type RSPTreeComponentProps = {
  list: FieldType[];
  depth?: number;
  permissionEdit?: PermissionEdit;
  setState: (d: FieldType[], t?: FieldType) => void;
  onExpand?: () => void;
};

const Tree: React.FC<RSPTreeComponentProps> = ({
  list,
  setState,
  depth = 1,
  permissionEdit,
}) => {
  const [expandedItems, setExpandedItems] = useState<ExpandedItems>({});
  const prevIsNewRole = useRef(false);
  const onCheck = useCallback(
    ix => (e: React.FormEvent<HTMLInputElement>) => {
      const newList = [...list] as FieldType[];
      const target = e.target as HTMLInputElement;
      newList[ix] = { ...list[ix], checked: target.checked };
      setState([...newList], newList[ix]);
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
    ix => (newState: FieldType[], field?: FieldType) => {
      let newList = [...list];
      newList[ix] = { ...list[ix], children: [...newState] };

      if (field && field.checked) newList = addDepFields(newList, field);

      setState([...newList]);
    },
    [setState, list]
  );

  useEffect(() => {
    const expandedItemsFromList = getExpandedItems(list);
    setExpandedItems({ ...expandedItems, ...expandedItemsFromList }); // this will only handle expand, it wont collapse anything which are already expanded.
  }, [list]);

  useEffect(() => {
    if (
      permissionEdit &&
      (!permissionEdit?.isNewRole ||
        (permissionEdit?.isNewRole &&
          permissionEdit?.isNewRole !== prevIsNewRole.current))
    ) {
      // ignore the new role name change event
      setExpandedItems({});
      prevIsNewRole.current = permissionEdit.isNewRole;
    }
  }, [permissionEdit]);

  const toggleExpand = (ix: number) => () => {
    setExpandedItems(oldExpandedItems => {
      const newState = !oldExpandedItems[ix];
      const newExpandeditems = { ...oldExpandedItems, [ix]: newState };
      return newExpandeditems;
    });
  };

  return (
    <ul className="pl-md">
      {list.map(
        (i: FieldType, ix) =>
          !i.name.startsWith('enum') &&
          !i.name.startsWith('scalar') && (
            <li key={i.name} className={styles.treeNodes}>
              {i.checked !== undefined && (
                <input
                  type="checkbox"
                  id={i.name}
                  name={i.name}
                  checked={i.checked}
                  data-test={`checkbox-${i.name}`}
                  onChange={onCheck(ix)}
                  className="legacy-input-fix"
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
                expanded={expandedItems[ix]}
              />
              {i.children && expandedItems[ix] && (
                <MemoizedTree
                  list={i.children}
                  depth={depth + 1}
                  setState={setValue(ix)}
                />
              )}
            </li>
          )
      )}
    </ul>
  );
};

const MemoizedTree = React.memo(Tree);
export default MemoizedTree;
