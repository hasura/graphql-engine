import React, {
  useCallback,
  useEffect,
  useState,
} from 'react';
import * as GQL from 'graphql';
import _ from 'lodash';
import { generateSDL, generateConstantTypes,  getArgTreeFromPermissionSDL } from './utils';
import Button from '../../../Common/Button/Button';
import styles from '../../../Common/Permissions/PermissionStyles.scss';
import { DatasourceObject, FieldType } from './types';
import { Field } from './Field';
import { PermissionEditorContext } from './context';

interface RSPTreeComponentProps {
  list: FieldType[];
  setState: (d: FieldType[]) => void;
}
type ExpandedItems = {
  [key: string]: boolean;
};

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
// TODO seperate components


// TODO seperate components


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
    schema,
  } = props;

  const [state, setState] = React.useState<DatasourceObject[]>(datasource); // TODO - low priority:  a copy of datasource, could be able to remove this after evaluation
  const [argTree, setArgTree] = React.useState({}); // all @presets as an object tree
  const [resultString, setResultString] = React.useState(''); // Generated SDL

  const { isNewRole, isNewPerm } = permissionEdit;

  useEffect(() => {
    window.SCHEMA = schema;
    window.GQL = GQL;

    console.log('changed--->', state);
    if (!state) return;
    setResultString(generateSDL(state, argTree));
    // setSchemaDefinition(resultString);
  }, [state, argTree]);

  useEffect(() => {
    setState(datasource);
    setResultString(schemaDefinition);
  }, [datasource]);

  useEffect(() => {
    if (!!schemaDefinition) {
      try {
        const newArgTree = getArgTreeFromPermissionSDL(schemaDefinition)
        setArgTree(newArgTree)
      } catch (e) {
        console.error(e)
      }
    }
  }, [schemaDefinition])

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
    const finalString = resultString + generateConstantTypes(schema);
    setSchemaDefinition(finalString);
    save();
  };

  const removeFunc = () => {
    removeRemoteSchemaPermission(closeEditor);
  };

  return (
    <div className={styles.activeEdit}>
      <div className={styles.tree}>
        <PermissionEditorContext.Provider value={{ argTree, setArgTree }}>
          <MemoizedTree list={state as FieldType[]} setState={setState} />
          {/* <code s tyle={{ whiteSpace: 'pre-wrap' }}>{resultString}</code> */}
        </PermissionEditorContext.Provider>
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

export default React.memo(PermissionEditor);
