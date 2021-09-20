import React, {
  useCallback,
  useContext,
  useEffect,
  useState,
  MouseEvent,
} from 'react';
import { FieldType } from './types';
import { PermissionEditorContext } from './context';
import { CollapsedField } from './CollapsedField';
import { ArgSelect } from './ArgSelect';
import { isEmpty } from '../../../Common/utils/jsUtils';
import styles from '../../../Common/Permissions/PermissionStyles.scss';
import { generateTypeString, getChildArguments } from './utils';
import Pen from './Pen';

export interface FieldProps {
  i: FieldType;
  setItem: (e: FieldType) => void;
  onExpand?: () => void;
  expanded: boolean;
}

export const Field: React.FC<FieldProps> = ({
  i,
  setItem = e => console.log(e),
  onExpand = console.log,
  expanded,
}) => {
  const [inputPresetMode, setInputPresetMode] = useState<boolean>(false);
  const [autoExpandInputPresets, setAutoExpandInputPresets] = useState<boolean>(
    false
  );
  const context: any = useContext(PermissionEditorContext);
  let initState;
  if (i.parentName) {
    initState = context.argTree?.[i.parentName]?.[i.name]
      ? { ...context.argTree[i.parentName][i.name] }
      : {};
  } else {
    initState = context.argTree?.[i.name] ? { ...context.argTree[i.name] } : {};
  }
  const [fieldVal, setfieldVal] = useState<Record<string, any>>(initState);
  const setArg = useCallback(
    (vStr: Record<string, unknown>) => {
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
  useEffect(() => {
    // auto expand args for InputObjectTypes when there is prefilled values
    // happens only first time when the node is created
    if (
      fieldVal &&
      fieldVal !== {} &&
      Object.keys(fieldVal).length > 0 &&
      !isEmpty(fieldVal) &&
      !autoExpandInputPresets
    ) {
      setAutoExpandInputPresets(true);
      setInputPresetMode(true);
    }
  }, [autoExpandInputPresets]);

  useEffect(() => {
    if (
      fieldVal &&
      fieldVal !== {} &&
      Object.keys(fieldVal).length > 0 &&
      !isEmpty(fieldVal)
    ) {
      context.setArgTree((argTree: Record<string, any>) => {
        const tree = i.parentName
          ? {
              ...argTree,
              [i.parentName]: { ...argTree[i.parentName], [i.name]: fieldVal },
            }
          : { ...argTree, [i.name]: fieldVal };
        return tree;
      });
    }
  }, [fieldVal]);

  const handleClick = (e: MouseEvent<HTMLAnchorElement>) => {
    e.preventDefault();
    const target = e.target as HTMLAnchorElement;
    const selectedTypeName = target.id;

    // context from PermissionEditor.tsx
    context.scrollToElement(selectedTypeName);
  };

  if (!i.checked)
    return (
      <CollapsedField
        field={i}
        onClick={handleClick}
        onExpand={onExpand}
        expanded={expanded}
      />
    );

  const isFirstLevelInputObjPreset =
    i.isInputObjectType &&
    inputPresetMode &&
    getChildArguments(i.args[i.name]) &&
    Object.keys(getChildArguments(i.args[i.name])).length === 0;

  return (
    <>
      <span
        className={`${styles.padd_small_left} ${styles.fw_large}`}
        id={i.name}
      >
        {i.name}
      </span>
      {(i.isInputObjectType &&
        inputPresetMode &&
        !isFirstLevelInputObjPreset) ||
      !i.isInputObjectType ? (
        <>
          {i.args && Object.keys(i.args).length !== 0 && ' ('}
          {i.args && (
            <ul data-test={i.name}>
              {i.args &&
                Object.entries(i.args).map(([k, v]) => (
                  <ArgSelect
                    key={k}
                    keyName={k}
                    valueField={v}
                    value={fieldVal[k]}
                    setArg={setArg}
                    level={0}
                    isInputObjectType={i.isInputObjectType}
                  />
                ))}
            </ul>
          )}
          {i.args && Object.keys(i.args).length !== 0 && ' )'}
        </>
      ) : null}
      {i.return && (
        <span className={styles.fw_large}>
          :
          <a
            onClick={handleClick}
            id={generateTypeString(i.return || '')}
            href={`./permissions#${generateTypeString(i.return || '')}`}
          >
            {i.return}
          </a>
        </span>
      )}
      {/* show pen icon for input object types presets */}
      {i.isInputObjectType && !inputPresetMode && !autoExpandInputPresets ? (
        <button onClick={() => setInputPresetMode(true)}>
          <Pen />
        </button>
      ) : null}
      {i.isInputObjectType && inputPresetMode && isFirstLevelInputObjPreset ? (
        <ArgSelect
          key={i.name}
          keyName={i.name}
          valueField={i.args[i.name]}
          value={fieldVal[i.name]}
          setArg={setArg}
          level={0}
          isInputObjectType={i.isInputObjectType}
          isFirstLevelInputObjPreset
        />
      ) : null}
    </>
  );
};
