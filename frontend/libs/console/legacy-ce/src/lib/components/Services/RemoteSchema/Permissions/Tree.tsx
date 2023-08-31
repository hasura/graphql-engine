import React, { useCallback, useEffect, useRef, useState } from 'react';
import { FieldType, ExpandedItems, PermissionEdit } from './types';
import { Field } from './Field';
import { addDepFields, getExpandedItems } from './utils';
import { FaChevronDown, FaChevronRight } from 'react-icons/fa';
import { Checkbox } from '../../../../new-components/Form';
import { CheckedState } from '@radix-ui/react-checkbox';

type RSPTreeComponentProps = {
  list: FieldType[];
  filteredTypes: Set<string> | null;
  depth?: number;
  permissionEdit?: PermissionEdit;
  setState: (d: FieldType[], t?: FieldType) => void;
  onExpand?: () => void;
  onToggleAll?: () => void;
};

const Tree: React.FC<RSPTreeComponentProps> = ({
  filteredTypes,
  list,
  setState,
  depth = 1,
  permissionEdit,
  onToggleAll,
}) => {
  const [expandedItems, setExpandedItems] = useState<ExpandedItems>({});
  const prevIsNewRole = useRef(false);

  const onCheck = useCallback(
    ix => (event: CheckedState) => {
      const newList = [...list] as FieldType[];
      newList[ix] = { ...list[ix], checked: event as boolean };
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

  const toggleAll = useCallback(
    ix => () => {
      let newList = [...list];
      const children = newList[ix].children;

      if (!children) {
        return;
      }
      const allChecked = children.every(i => i.checked);
      list[ix].children?.forEach((i, ix2) => {
        children[ix2] = { ...i, checked: !allChecked };
        if (!allChecked) {
          newList = addDepFields(newList, children[ix2]);
        }
      });
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
    <ul
      className={`pl-md  border-gray-300 border-dotted ${
        depth === 1 ? '' : 'border-l-2'
      }`}
    >
      {depth > 1 && (
        <div className="pt-2 pb-1">
          <span className="text-gray-400">Fields</span>
          <button className="ml-8 text-[#337ab7]" onClick={onToggleAll}>
            {list.every(i => i.checked) ? 'Unselect All' : 'Select All'}
          </button>
        </div>
      )}
      {list.map((i: FieldType, ix) => {
        if (filteredTypes && !filteredTypes.has(i.name)) {
          return null;
        }
        return (
          !i.name.startsWith('enum') &&
          !i.name.startsWith('scalar') && (
            <li className="my-1" key={i.name}>
              <div className="flex items-start">
                {i.checked !== undefined && (
                  <Checkbox
                    name={i.name}
                    checked={i.checked}
                    data-test={`checkbox-${i.name}`}
                    onCheckedChange={onCheck(ix)}
                    className="inline-flex"
                  />
                )}
                {i.children && (
                  <button onClick={toggleExpand(ix)}>
                    {expandedItems[ix] ? (
                      <FaChevronDown size={10} />
                    ) : (
                      <FaChevronRight size={10} />
                    )}
                  </button>
                )}
                <div>
                  <Field
                    i={i}
                    setItem={setItem(ix)}
                    key={i.name}
                    onExpand={toggleExpand(ix)}
                    expanded={expandedItems[ix]}
                    items={i.children}
                  />
                </div>
              </div>
              {i.children && expandedItems[ix] && (
                <MemoizedTree
                  filteredTypes={null}
                  list={i.children}
                  depth={depth + 1}
                  setState={setValue(ix)}
                  onToggleAll={toggleAll(ix)}
                />
              )}
            </li>
          )
        );
      })}
    </ul>
  );
};

const MemoizedTree = React.memo(Tree);
export default MemoizedTree;
