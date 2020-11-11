import React from 'react';
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
  const onCheck = (ix: any) => (e: any) => {
    const newList = [...list];
    newList[ix] = { ...list[ix], checked: e.target.checked };
    setState([...newList]);
  };
  const setValue = (ix: any) => (newState: any) => {
    const newList = [...list];
    newList[ix] = { ...list[ix], children: [...newState] };
    setState([...newList]);
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
          <Item i={i} />
          {i.children && <Tree list={i.children} setState={setValue(ix)} />}
        </li>
      ))}
    </ul>
  );
};

const Item = ({ i }: any) => {
  return (
    <b>
      {i.name}
      {i.args && ' ('}
      {i.args &&
        Object.entries(i.args).map(([k, v]) => <Select {...{ k, v }} />)}
      {i.args && ' )'}
      {i.return && `: ${i.return}`}
    </b>
  );
};

const Select = ({ k, v }: any) => {
  return (
    <>
      <label htmlFor={k}> {k}:</label>
      <select name={k} id={k} value={v.type}>
        <option value="staticVal">static value</option>
        <option value="sessionVar">from session var</option>
      </select>
      <input value={v.value} />
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
  } = props;

  const { isNewRole, isNewPerm } = permissionEdit;

  const [state, setState] = React.useState(data);
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
