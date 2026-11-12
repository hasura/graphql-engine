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
import { isEmpty } from '@hasura/shared/utils';
import { generateTypeString, getChildArguments } from './utils';
import Pen from './Pen';
import { FaChevronDown, FaChevronRight, FaFilter } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import { Button, IconButton, Link, Text } from '@hasura/shared/ui';

interface FieldProps {
  i: FieldType;
  setItem: (e: FieldType) => void;
  onExpand?: () => void;
  expanded: boolean;
  items?: FieldType[];
}

export const Field: React.FC<FieldProps> = ({
  i,
  setItem = (e) => () => {
    /* noop */
  },
  onExpand = () => {
    /* noop */
  },
  expanded,
  items,
}) => {
  const [argumentsExpanded, setArgumentsExpanded] = useState<boolean>(true);
  const context: any = useContext(PermissionEditorContext);
  const { setArgTree } = context;
  let initState;
  if (i.parentName) {
    initState = context.argTree?.[i.parentName]?.[i.name]
      ? { ...context.argTree[i.parentName][i.name] }
      : {};
  } else {
    initState = context.argTree?.[i.name] ? { ...context.argTree[i.name] } : {};
  }
  // auto expand args for InputObjectTypes when there are prefilled values.
  // This "happens only first time when the node is created", so instead of
  // seeding the state from an effect (which caused a cascading render) we
  // compute the seed once via lazy initial state.
  const hasPrefilledPresets =
    !!initState && Object.keys(initState).length > 0 && !isEmpty(initState);
  const [inputPresetMode, setInputPresetMode] =
    useState<boolean>(hasPrefilledPresets);
  // Locked at mount (lazy init): the previous effect only seeded this once when
  // the node was created and never reacted to later `fieldVal` changes.
  const [autoExpandInputPresets] = useState<boolean>(hasPrefilledPresets);
  const [fieldVal, setfieldVal] = useState<Record<string, any>>(initState);
  const setArg = useCallback((vStr: Record<string, unknown>) => {
    setfieldVal((oldVal) => {
      const newState = {
        ...oldVal,
        ...vStr,
      };
      return newState;
    });
  }, []);

  useEffect(() => {
    if (fieldVal && Object.keys(fieldVal).length > 0 && !isEmpty(fieldVal)) {
      setArgTree((argTree: Record<string, any>) => {
        const tree = i.parentName
          ? {
              ...argTree,
              [i.parentName]: { ...argTree[i.parentName], [i.name]: fieldVal },
            }
          : { ...argTree, [i.name]: fieldVal };
        return tree;
      });
    }
  }, [fieldVal, setArgTree, i.name, i.parentName]);

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
          <Text>
            {' '}
            {items.filter((item: any) => item.checked).length ?? 0} of{' '}
            {items.length ?? 0} Fields
          </Text>
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
      <Text weight="medium" id={i.name}>
        {i.name}
        {i.return && (
          <>
            <span> : </span>
            <Link
              onClick={handleClick}
              id={generateTypeString(i.return || '')}
              href={`./permissions#${generateTypeString(i.return || '')}`}
            >
              {i.return}
            </Link>
          </>
        )}
      </Text>
      {(i.isInputObjectType &&
        inputPresetMode &&
        !isFirstLevelInputObjPreset) ||
      !i.isInputObjectType ? (
        <div className="ml-4">
          {Object.keys(i?.args ?? {})?.length > 0 && (
            <>
              <Flex align="start">
                <Button
                  onClick={() => setArgumentsExpanded(!argumentsExpanded)}
                  variant="ghost"
                  color="gray"
                  leftIcon={argumentsExpanded ? FaChevronDown : FaChevronRight}
                >
                  Arguments
                </Button>
                {!argumentsExpanded &&
                  Object.keys(i.args).some((key) => !!fieldVal[key]) && (
                    <FaFilter className="relative ml-2" size={10} />
                  )}
              </Flex>
              {argumentsExpanded && (
                <ul data-test={i.name} className="ml-8">
                  {i.args && (
                    <>
                      {Object.entries(i.args).map(([k, v]) => (
                        <div key={k} className="my-1">
                          <ArgSelect
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
        <IconButton
          color="gray"
          radius="full"
          variant="ghost"
          icon={Pen}
          onClick={() => setInputPresetMode(true)}
        />
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
