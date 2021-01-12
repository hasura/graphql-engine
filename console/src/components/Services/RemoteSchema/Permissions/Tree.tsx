import React, { useCallback, useState } from 'react';
import { GraphQLScalarType, GraphQLInputObjectType } from 'graphql';
import { FieldType, RSPTreeComponentProps, ExpandedItems } from './types';
import { Field } from './Field';
import { getTrimmedReturnType } from './utils';

const getDeps = (field: FieldType, res = new Set<string>([])) => {
  console.log(field);
  if (field.return) res.add(getTrimmedReturnType(field.return));

  // args
  if (field.args)
    Object.values(field.args).forEach(arg => {
      // TODO filtering logic
      if (!(arg.type instanceof GraphQLScalarType)) {
        const subType = getTrimmedReturnType(arg.type.inspect());
        res.add(subType);
      }
      console.log(arg);
    });

  if (field.type && field?.type instanceof GraphQLInputObjectType)
    Object.values(field.type.getFields()).forEach(arg => {
      console.log(arg)
      // TODO filtering logic
      if (!(arg.type instanceof GraphQLScalarType)) {
        const subType = getTrimmedReturnType(arg.type.inspect());
        res.add(subType);
      }
    });

  return res;
};

const addDepFields = (list: FieldType[], field: FieldType) => {
  const deps = getDeps(field);
  const newList = list.map((fld: FieldType) => {
    const newField = { ...fld };
    if (fld.typeName && deps.has(fld.typeName))
      if (newField.children)
        newField.children = newField.children.map(ch => ({
          ...ch,
          checked: true,
        }));
    return newField;
  });
  return newList;
};

const Tree: React.FC<RSPTreeComponentProps> = ({
  list,
  setState,
  depth = 1,
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
      console.log(newState, depth, list, field);
      let newList = [...list];
      newList[ix] = { ...list[ix], children: [...newState] };
      if (field) newList = addDepFields(newList, field);

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
