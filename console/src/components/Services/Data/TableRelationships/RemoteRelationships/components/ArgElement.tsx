import React from 'react';
import { TreeArgElement, ArgValueKind } from '../utils';
import styles from '../SchemaExplorer.scss';
import ArgValueElement from './ArgValue';

type Props = {
  arg: TreeArgElement;
  handleToggle: (a: TreeArgElement) => void;
  handleArgValueKindChange: (a: TreeArgElement, type: ArgValueKind) => void;
  handleArgValueChange: (a: TreeArgElement, value: string) => void;
  columns: string[];
};

const ArgElement: React.FC<Props> = ({
  arg,
  handleToggle,
  handleArgValueChange,
  handleArgValueKindChange,
  columns,
}) => {
  const style = {
    marginLeft: `${(arg.depth + arg.parentFieldDepth) * 20 + 20}px`,
    color: '#8B2BB9',
    fontStyle: 'italic',
  };
  const toggle = () => handleToggle(arg);
  return (
    <div
      className={`${styles.display_flex} ${styles.add_mar_bottom_mid}`}
      style={style}
    >
      <div className={styles.add_mar_right_small}>
        <input
          checked={arg.isChecked}
          type="checkbox"
          className={styles.cursorPointer}
          onChange={toggle}
        />
      </div>
      <div
        className={`${styles.add_mar_right_small} ${styles.cursorPointer}`}
        onClick={toggle}
        role="checkbox"
        aria-checked={arg.isChecked}
      >
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
