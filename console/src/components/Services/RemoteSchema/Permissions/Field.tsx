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
import { generateTypeString } from './utils';

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
  const context: any = useContext(PermissionEditorContext);
  const initState =
    context.argTree && context.argTree[i.name]
      ? { ...context.argTree[i.name] }
      : {};
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
    if (
      fieldVal &&
      fieldVal !== {} &&
      Object.keys(fieldVal).length > 0 &&
      !isEmpty(fieldVal)
    ) {
      context.setArgTree((argTree: Record<string, any>) => {
        return { ...argTree, [i.name]: fieldVal };
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
  return (
    <>
      <span
        className={`${styles.padd_small_left} ${styles.fw_large}`}
        id={i.name}
      >
        {i.name}
      </span>
      {i.args && ' ('}
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
              />
            ))}
        </ul>
      )}
      {i.args && ' )'}
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
    </>
  );
};
