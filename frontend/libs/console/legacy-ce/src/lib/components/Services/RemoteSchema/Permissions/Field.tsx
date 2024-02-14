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
import { generateTypeString, getChildArguments } from './utils';
import Pen from './Pen';
import { FaChevronDown, FaChevronRight, FaFilter } from 'react-icons/fa';

export interface FieldProps {
  i: FieldType;
  setItem: (e: FieldType) => void;
  onExpand?: () => void;
  expanded: boolean;
  items?: FieldType[];
}

export const Field: React.FC<FieldProps> = ({
  i,
  setItem = e => () => {},
  onExpand = () => {},
  expanded,
  items,
}) => {
  const [inputPresetMode, setInputPresetMode] = useState<boolean>(false);
  const [argumentsExpanded, setArgumentsExpanded] = useState<boolean>(true);
  const [autoExpandInputPresets, setAutoExpandInputPresets] =
    useState<boolean>(false);
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
      Object.keys(fieldVal).length > 0 &&
      !isEmpty(fieldVal) &&
      !autoExpandInputPresets
    ) {
      setAutoExpandInputPresets(true);
      setInputPresetMode(true);
    }
  }, [autoExpandInputPresets]);

  useEffect(() => {
    if (fieldVal && Object.keys(fieldVal).length > 0 && !isEmpty(fieldVal)) {
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
      <>
        <CollapsedField
          field={i}
          onClick={handleClick}
          onExpand={onExpand}
          expanded={expanded}
        />
        {items && (
          <span className="text-gray-400 pt-4 pb-2 pl-6">
            {items.filter((item: any) => item.checked).length ?? 0} of{' '}
            {items.length ?? 0} Fields
          </span>
        )}
      </>
    );

  const isFirstLevelInputObjPreset =
    i.isInputObjectType &&
    inputPresetMode &&
    getChildArguments(i.args[i.name]) &&
    Object.keys(getChildArguments(i.args[i.name])).length === 0;

  return (
    <>
      <span className="pl-xs font-semibold" id={i.name}>
        {i.name}
        {i.return && (
          <span className="font-semibold">
            {' '}
            :{' '}
            <a
              onClick={handleClick}
              id={generateTypeString(i.return || '')}
              href={`./permissions#${generateTypeString(i.return || '')}`}
            >
              {i.return}
            </a>
          </span>
        )}
      </span>
      {(i.isInputObjectType &&
        inputPresetMode &&
        !isFirstLevelInputObjPreset) ||
      !i.isInputObjectType ? (
        <div className="ml-4">
          {Object.keys(i?.args ?? {})?.length > 0 && (
            <>
              <div className="flex items-center text-gray-400 mt-1">
                <button
                  onClick={() => setArgumentsExpanded(!argumentsExpanded)}
                >
                  {argumentsExpanded ? (
                    <FaChevronDown className="relative -top-0.5" size={10} />
                  ) : (
                    <FaChevronRight className="relative -top-0.5" size={10} />
                  )}
                  <span className="pl-xs">Arguments</span>
                </button>
                {!argumentsExpanded &&
                  Object.keys(i.args).some(key => !!fieldVal[key]) && (
                    <FaFilter className="relative ml-2" size={10} />
                  )}
              </div>
              {argumentsExpanded && (
                <ul data-test={i.name} className="ml-8">
                  {i.args && (
                    <>
                      {Object.entries(i.args).map(([k, v]) => (
                        <div className="my-1">
                          <ArgSelect
                            key={k}
                            keyName={k}
                            valueField={v}
                            value={fieldVal[k]}
                            setArg={setArg}
                            level={0}
                            isInputObjectType={i.isInputObjectType}
                          />
                        </div>
                      ))}
                    </>
                  )}
                </ul>
              )}
            </>
          )}
        </div>
      ) : null}
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
