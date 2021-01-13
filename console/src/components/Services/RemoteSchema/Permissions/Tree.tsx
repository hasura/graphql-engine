import React, { useCallback, useState } from 'react';
import { GraphQLScalarType } from 'graphql';
import { FieldType, RSPTreeComponentProps, ExpandedItems } from './types';
import { Field } from './Field';
import { getTrimmedReturnType } from './utils';

const getDeps = (field: FieldType, res = new Set<string>([])) => {
  if (field.return) res.add(getTrimmedReturnType(field.return));

  // args
  if (field.args)
    Object.values(field.args).forEach(arg => {
      // TODO filtering logic
      if (!(arg.type instanceof GraphQLScalarType)) {
        const subType = getTrimmedReturnType(arg.type.inspect());
        res.add(subType);
      }
    });

  return res;
};

const addTypesRecursively = (
  list: FieldType[],
  typeList: Set<string>,
  alreadyChecked: Array<string>
): FieldType[] => {
  // if alreadychecked has then remove from typelist, if not then add to alreadychecked
  alreadyChecked.forEach(key => {
    if (typeList.has(key)) {
      typeList.delete(key);
    }
  });
  typeList.forEach(value => {
    alreadyChecked.push(value);
  });

  // exit condition
  // if typelist is empty
  if (typeList.size === 0) return list;

  const newList = list.map((fld: FieldType) => {
    const newField = { ...fld };
    if (fld.typeName && typeList.has(fld.typeName)) {
      if (newField.children) {
        const partiallyChecked = newField.children.find(({ checked }) => {
          if (checked) return true;
          return false;
        });
        if (!partiallyChecked)
          newField.children = newField.children.map(ch => {
            if (ch.return) typeList.add(getTrimmedReturnType(ch.return));
            return {
              ...ch,
              checked: true,
            };
          });
      }
    }
    return newField;
  });
  return addTypesRecursively(newList, typeList, alreadyChecked);
};

const addDepFields = (list: FieldType[], field: FieldType) => {
  const deps = getDeps(field);
  const alreadyChecked: Array<string> = [];
  const newList = addTypesRecursively(list, deps, alreadyChecked);
  return newList;
};

const Tree: React.FC<RSPTreeComponentProps> = ({
  list,
  setState,
  depth = 1,
}) => {
  const [expandedItems, setExpandedItems] = useState<ExpandedItems>({});
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
              depth={depth + 1}
              setState={setValue(ix)}
            />
          )}
        </li>
      ))}
    </ul>
  );
};

const MemoizedTree = React.memo(Tree);
export default MemoizedTree;
