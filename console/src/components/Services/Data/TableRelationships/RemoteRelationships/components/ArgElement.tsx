import React from 'react';
import { TreeArgElement, ArgValueKind } from '../utils';
import styles from '../SchemaExplorer.scss';
import ArgValueElement from './ArgValue';
import { HasuraColumn } from './Explorer';

type Props = {
  arg: TreeArgElement;
  handleToggle: (a: TreeArgElement) => void;
  handleArgValueKindChange: (a: TreeArgElement, type: ArgValueKind) => void;
  handleArgValueChange: (a: TreeArgElement, value: string) => void;
  columns: HasuraColumn;
};

const ArgElement: React.FC<Props> = ({
  arg,
  handleToggle,
  handleArgValueChange,
  handleArgValueKindChange,
  columns,
}) => {
  const toggle = () => handleToggle(arg);
  return (
    <div
      style={{
        marginLeft: `${(arg.depth + arg.parentFieldDepth) * 20 + 20}px`,
      }}
      className={`${styles.display_flex} ${styles.add_mar_bottom_mid} ${styles.argElement}`}
    >
      <div
        className={`${styles.add_mar_right_small} ${styles.cursorPointer}`}
        onClick={toggle}
        role="checkbox"
        aria-checked={arg.isChecked}
      >
        <input
          checked={arg.isChecked}
          type="checkbox"
          className={`${styles.add_mar_right_small} ${styles.cursorPointer} legacy-input-fix`}
        />
        {arg.name}
      </div>
      {arg.isChecked && arg.isLeafArg && (
        <ArgValueElement
          value={arg.value}
          handleArgValueKindChange={e =>
            handleArgValueKindChange(arg, e.target.value as ArgValueKind)
          }
          handleArgValueChange={e => handleArgValueChange(arg, e.target.value)}
          columns={columns}
        />
      )}
    </div>
  );
};

export default ArgElement;
