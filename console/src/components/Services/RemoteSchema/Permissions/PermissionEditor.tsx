import React, { useCallback, useState } from 'react';
import Button from '../../../Common/Button/Button';
import styles from '../../../Common/Permissions/PermissionStyles.scss';

const data = [
  {
    name: 'Query_root',
    children: [
      { name: 'hello', checked: false },
      {
        name: 'user',
        checked: false,
        return: 'User',
        args: {
          id: {
            type: 'sessionVar',
            value: 'x-hasura-user-id',
          },
          name: {
            type: 'sessionVar',
            value: 'x-hasura-user-id',
          },
        },
      },
    ],
  },
  {
    name: 'Mutation_root',
    children: [
      { name: 'set_hello', checked: false },
      {
        name: 'user',
        return: 'User',
        checked: false,
        args: {
          id: {
            type: 'sessionVar',
            value: 'x-hasura-user-id',
          },
          name: {
            type: 'sessionVar',
            value: 'x-hasura-user-id',
          },
        },
      },
    ],
  },
  {
    name: 'Subscription_root',
    children: [
      { name: 'hello', checked: false },
      {
        name: 'user',
        checked: true,
        return: 'User',
        args: {
          id: {
            type: 'sessionVar',
            value: 'x-hasura-user-id',
          },
          name: {
            type: 'sessionVar',
            value: 'x-hasura-user-id',
          },
        },
      },
    ],
  },
  {
    name: 'Type user',
    children: [
      { name: 'hello', return: 'String' },
      { name: 'user', return: 'User' },
    ],
  },
];

const Tree = ({ list, setState }: any) => {
  // TODO add checkbox
  // TODO create and sync tree
  // TODO check actual gql schema structure and change, if required
  const [expandedItems, setExpandedItems] = useState({});
  const onCheck = useCallback(
    ix => e => {
      const newList = [...list];
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
    ix => newState => {
      const newList = [...list];
      newList[ix] = { ...list[ix], children: [...newState] };
      setState([...newList]);
    },
    [setState, list]
  );
  const toggleExpand = ix => e => {
    setExpandedItems(expandedItems => {
      const newState = !expandedItems[ix];
      const newExpandeditems = { ...expandedItems, [ix]: newState };
      return newExpandeditems;
    });
  };
  return (
    <ul>
      {list.map((i, ix) => (
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
            <button
              onClick={toggleExpand(ix)}
              style={{
                backgroundColor: 'transparent',
                border: 0,
                color: '#0008',
                cursor: 'pointer',
              }}
            >
              {expandedItems[ix] ? '-' : '+'}
            </button>
          )}
          <Item i={i} setItem={setItem(ix)} />
          {i.children && expandedItems[ix] && (
            <Tree list={i.children} setState={setValue(ix)} />
          )}
        </li>
      ))}
    </ul>
  );
};

const Item = ({ i, setItem = e => console.log(e) }) => {
  const setArg = useCallback(
    k => v => setItem({ ...i, args: { ...i.args, [k]: v } }),
    [setItem, i]
  );
  return (
    <b>
      <b id={i.name}>{i.name}</b>
      {i.args && ' ('}
      {i.args &&
        Object.entries(i.args).map(([k, v]) => (
          <Select {...{ k, v, setArg: setArg(k) }} />
        ))}
      {i.args && ' )'}
      {i.return && (
        <>
          :{' '}
          <a href={`#type_${i.return.replace(/[^\w\s]/gi, '')}`}>{i.return}</a>
        </>
      )}
    </b>
  );
};

const Select = ({ k, v, setArg = e => console.log(e) }) => {
  const setArgVal = useCallback(d => setArg({ ...v, value: d }), [setArg, v]);
  return (
    <>
      <label htmlFor={k}> {k}:</label>
      <input
        value={v.value}
        style={{ border: 0, borderBottom: '2px solid #354c9d' }}
        onChange={e => setArgVal(e.target.value)}
      />
    </>
  );
};

const PermissionEditor = ({ ...props }: any) => {
  const {
    permissionEdit,
    isEditing,
    isFetching,
    schemaDefinition,
    // readOnlyMode,
    permCloseEdit,
    saveRemoteSchemaPermission,
    removeRemoteSchemaPermission,
    setSchemaDefinition,
    jsonTree,
  } = props;

  const { isNewRole, isNewPerm } = permissionEdit;

  const [state, setState] = React.useState(jsonTree);
  React.useEffect(() => {
    console.log('changed--->', state);
  }, [state]);
  const sets = (d: any) => {
    setState([...d]);
  };

  if (!isEditing) return null;

  const {
    value: schemaDefinitionSdl,
    error: schemaDefinitionError,
    timer: schemaParseTimer,
  } = schemaDefinition;

  console.log('in perm editor: ', schemaDefinitionSdl);

  // TODO : types
  const schemaDefinitionOnChange = (
    value: any,
    error: any,
    timer: any,
    ast: any
  ) => {
    setSchemaDefinition({ value, error, timer, ast });
  };

  const buttonStyle = styles.add_mar_right;

  const closeEditor = () => {
    permCloseEdit();
  };

  const saveFunc = () => {
    saveRemoteSchemaPermission(closeEditor);
  };

  const removeFunc = () => {
    removeRemoteSchemaPermission(closeEditor);
  };

  return (
    <div className={styles.activeEdit}>
      <Tree list={state} setState={sets} />
      <Button
        onClick={saveFunc}
        color="yellow"
        className={buttonStyle}
        disabled={isFetching}
      >
        Save Permissions
      </Button>
      {!(isNewRole || isNewPerm) && (
        <Button
          onClick={removeFunc}
          color="red"
          className={buttonStyle}
          disabled={isFetching}
        >
          Remove Permissions
        </Button>
      )}
      <Button color="white" className={buttonStyle} onClick={closeEditor}>
        Cancel
      </Button>
    </div>
  );
};

export default PermissionEditor;
