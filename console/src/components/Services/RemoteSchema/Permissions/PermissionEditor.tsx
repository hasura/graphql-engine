import React, {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useRef,
  useState,
} from 'react';
import * as GQL from 'graphql';
import _ from 'lodash';
import Pen from './Pen';
import { generateSDL, getChildArgument } from './utils';
import Button from '../../../Common/Button/Button';
import styles from '../../../Common/Permissions/PermissionStyles.scss';

const RootContext = createContext();

const Tree = ({ list, setState }) => {
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
            <button onClick={toggleExpand(ix)}>
              {expandedItems[ix] ? '-' : '+'}
            </button>
          )}
          <Field i={i} setItem={setItem(ix)} key={i.name} />
          {i.children && expandedItems[ix] && (
            <Tree list={i.children} setState={setValue(ix)} />
          )}
        </li>
      ))}
    </ul>
  );
};
const CollapsedField = ({ field: i, onClick }) => (
  <>
    <b id={i.name}>{i.name}</b>
    {i.return && (
      <b>
        :
        <a
          onClick={onClick}
          id={`${i.return.replace(/[^\w\s]/gi, '')}`}
          href={`${i.return.replace(/[^\w\s]/gi, '')}`}
        >
          {i.return}
        </a>
      </b>
    )}
  </>
);

const Field = ({ i, setItem = e => console.log(e) }) => {
  const [fieldVal, setfieldVal] = useState({});
  const setArg = useCallback(
    (k, v) => vStr => {
      setfieldVal(oldVal => {
        const newState = {
          ...oldVal,
          ...vStr,
        };
        return newState;
      });
    },
    [setItem, i]
  );
  const cntxt: any = useContext(RootContext);
  useEffect(() => {
    if (fieldVal && fieldVal !== {} && Object.keys(fieldVal).length > 0) {
      cntxt.setArgTree(argTree => {
        return { ...argTree, [i.name]: fieldVal };
      });
    }
  }, [fieldVal]);

  const handleClick = e => {
    e.preventDefault();
    const selectedTypeName = e.target.id;
    const selectedUrl = e.target.href;
    console.log('selected url: ', selectedUrl);
  };

  if (!i.checked) return <CollapsedField field={i} onClick={handleClick} />;
  return (
    <b>
      <b id={i.name}>{i.name}</b>
      {i.args && ' ('}
      <ul>
        {i.args &&
          Object.entries(i.args).map(([k, v]) => (
            <ArgSelect
              {...{
                key: k,
                k,
                v,
                value: fieldVal[k],
                setArg: setArg(k, v),
                level: 0,
              }}
            />
          ))}
      </ul>
      <ul>
        {i.args && ' )'}
        {i.return && (
          <>
            :
            <a
              onClick={handleClick}
              id={`${i.return.replace(/[^\w\s]/gi, '')}`}
              href={`${i.return.replace(/[^\w\s]/gi, '')}`}
            >
              {i.return}
            </a>
          </>
        )}
      </ul>
    </b>
  );
};

const ArgSelect = ({ k, v, value, level, setArg = e => console.log(e) }) => {
  const [expanded, setExpanded] = useState(false);
  const [editMode, setEditMode] = useState(
    value && typeof value === 'string' && value.length > 0
  );
  const prevState = useRef();
  useEffect(() => {
    if (value && typeof value === 'string' && value.length > 0 && !editMode) {
      // show value instead of pen icon, if the value is defined in the prop
      setEditMode(true);
    }
  }, [value]);

  const { children, path } = getChildArgument(v);

  const setArgVal = val => {
    const prevVal = prevState.current;
    if (prevVal) {
      const newState = _.merge(prevVal, val);
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
      <ul style={{ paddingLeft: 0, marginLeft: '-8px' }}>
        <button onClick={toggleExpandMode} style={{}}>
          {expanded ? '-' : '+'}
        </button>
        <label
          style={{ cursor: 'pointer' }}
          htmlFor={k}
          onClick={toggleExpandMode}
        >
          {' '}
          {k}:
        </label>
        {expanded &&
          Object.values(children).map(i => {
            const childVal = value ? value[i?.name] : undefined;
            return (
              <li>
                <ArgSelect
                  {...{
                    k: i.name,
                    setArg: v => setArgVal({ [k]: v }),
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
      <label htmlFor={k}> {k}:</label>
      {editMode ? (
        <>
          <input
            value={value}
            style={{ border: 0, borderBottom: '2px solid #354c9d' }}
            onChange={e => setArgVal({ [v?.name]: e.target.value })}
          />
        </>
      ) : (
        <button onClick={() => setEditMode(true)}>
          <Pen />
        </button>
      )}
    </li>
  );
};

declare const window: any;

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
    datasource,
    schema
  } = props;

  const [state, setState] = React.useState(datasource); // TODO - low priority:  a copy of datasource, could be able to remove this after evaluation
  const [argTree, setArgTree] = React.useState({}); // all @presets as an object tree
  const [resultString, setResultString] = React.useState(''); // Generated SDL

  const { isNewRole, isNewPerm } = permissionEdit;

  useEffect(() => {

    window.SCHEMA = schema;
    window.GQL = GQL;

    console.log('changed--->', state);
    if (!state) return;
    // TODO make this a utility
    setResultString(generateSDL(state, argTree, schema));
    setSchemaDefinition(resultString);
  }, [state, argTree]);

  useEffect(() => {
    setState(datasource);
  }, [datasource]);

  if (!isEditing) return null;

  // const {
  //   value: schemaDefinitionSdl,
  //   error: schemaDefinitionError,
  //   timer: schemaParseTimer,
  // } = schemaDefinition;

  // console.log('in perm editor: ', schemaDefinitionSdl);

  // // TODO : types
  // const schemaDefinitionOnChange = (
  //   value: any,
  //   error: any,
  //   timer: any,
  //   ast: any
  // ) => {
  //   setSchemaDefinition({ value, error, timer, ast });
  // };

  const buttonStyle = styles.add_mar_right;

  const closeEditor = () => {
    permCloseEdit();
  };

  const save = () => {
    saveRemoteSchemaPermission(closeEditor);
  };

  const saveFunc = () => {
    setSchemaDefinition(resultString);
    save();
  };

  const removeFunc = () => {
    removeRemoteSchemaPermission(closeEditor);
  };

  return (
    <div className={styles.activeEdit}>
      <div className="tree">
        <RootContext.Provider value={{ argTree, setArgTree }}>
          <Tree list={state} setState={setState} />
          <code style={{ whiteSpace: 'pre-wrap' }}>{resultString}</code>
        </RootContext.Provider>
      </div>
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
