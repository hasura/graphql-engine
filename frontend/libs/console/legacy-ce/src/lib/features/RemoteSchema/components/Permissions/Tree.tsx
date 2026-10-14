import React, { useCallback, useEffect, useRef, useState } from 'react';
import { Flex } from '@radix-ui/themes';
import { FieldType, ExpandedItems } from './types';
import { Field } from './Field';
import { addDepFields, getExpandedItems } from './utils';
import { FaChevronDown, FaChevronRight } from 'react-icons/fa';
import {
  Button,
  Checkbox,
  CheckboxProps,
  IconButton,
  Text,
} from '@hasura/shared/ui';
import { PermissionEdit } from '../../hooks/useRemoteSchemaRelationshipForm';

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
  // Seed the expanded items from the initial list (lazy init) and merge in any
  // newly-expandable items whenever `list` changes (adjust-during-render,
  // below). This only ever expands items — it never collapses ones already
  // expanded — matching the previous effect's behaviour without the cascading
  // render that a setState-in-effect caused.
  const [expandedItems, setExpandedItems] = useState<ExpandedItems>(() =>
    getExpandedItems(list),
  );
  const [prevList, setPrevList] = useState(list);
  if (list !== prevList) {
    setPrevList(list);
    setExpandedItems((oldExpandedItems) => ({
      ...oldExpandedItems,
      ...getExpandedItems(list),
    }));
  }
  const prevIsNewRole = useRef(false);

  const onCheck = useCallback(
    (ix) => (event: CheckboxProps['value']) => {
      const newList = [...list] as FieldType[];
      newList[ix] = { ...list[ix], checked: event as boolean };
      setState([...newList], newList[ix]);
    },
    [setState, list],
  );

  const setItem = useCallback(
    (ix: number) => (newState: FieldType) => {
      const newList = [...list];
      newList[ix] = { ...newState };
      setState([...newList]);
    },
    [setState, list],
  );

  const setValue = useCallback(
    (ix) => (newState: FieldType[], field?: FieldType) => {
      let newList = [...list];
      newList[ix] = { ...list[ix], children: [...newState] };

      if (field && field.checked) newList = addDepFields(newList, field);
      setState([...newList]);
    },
    [setState, list],
  );

  const toggleAll = useCallback(
    (ix) => () => {
      let newList = [...list];
      const children = newList[ix].children;

      if (!children) {
        return;
      }
      const allChecked = children.every((i) => i.checked);
      list[ix].children?.forEach((i, ix2) => {
        children[ix2] = { ...i, checked: !allChecked };
        if (!allChecked) {
          newList = addDepFields(newList, children[ix2]);
        }
      });
      setState([...newList]);
    },
    [setState, list],
  );

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
    setExpandedItems((oldExpandedItems) => {
      const newState = !oldExpandedItems[ix];
      const newExpandeditems = { ...oldExpandedItems, [ix]: newState };
      return newExpandeditems;
    });
  };

  return (
    <ul
      className={`pl-4  border-gray-300 border-dotted ${
        depth === 1 ? '' : 'border-l-2'
      }`}
    >
      {depth > 1 && (
        <Flex className="pt-2 pb-1" align="center" gap="4">
          <Text>Fields</Text>
          <Button mode="primary" variant="ghost" onClick={onToggleAll}>
            {list.every((i) => i.checked) ? 'Unselect All' : 'Select All'}
          </Button>
        </Flex>
      )}
      {list.map((i: FieldType, ix) => {
        if (filteredTypes && !filteredTypes.has(i.name)) {
          return null;
        }
        return (
          !i.name.startsWith('enum') &&
          !i.name.startsWith('scalar') && (
            <li className="my-1" key={i.name}>
              <Flex align="center" gap="2">
                {i.checked !== undefined && (
                  <Checkbox
                    name={i.name}
                    value={i.checked}
                    data-test={`checkbox-${i.name}`}
                    onChange={onCheck(ix)}
                    className="inline-flex"
                  />
                )}
                {i.children && (
                  <IconButton
                    size="1"
                    color="gray"
                    variant="ghost"
                    onClick={toggleExpand(ix)}
                    icon={expandedItems[ix] ? FaChevronDown : FaChevronRight}
                  />
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
              </Flex>
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
