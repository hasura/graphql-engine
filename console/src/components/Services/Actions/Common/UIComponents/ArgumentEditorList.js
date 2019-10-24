import React from 'react';
import styles from './Styles.scss';
import Tooltip from './Tooltip';
import ArgumentEditor from './ArgumentEditor';

const editorLabel = 'Input Arguments';
const editorTooltip = 'These will be the arguments of the mutation';

const ArgumentsEditorList = ({
  className,
  allTypes,
  args,
  setArguments,
  service,
}) => {
  return (
    <div className={`${className || ''}`}>
      <h2
        className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
      >
        {editorLabel}
        <Tooltip
          id="action-name"
          text={editorTooltip}
          className={styles.add_mar_left_mid}
        />
      </h2>
      {args.map((a, i) => {
        const isLast = i === args.length - 1;
        const setArgument = arg => {
          const newArguments = JSON.parse(JSON.stringify(args));
          newArguments[i] = arg;
          setArguments(newArguments);
        };
        const removeArgument = () => {
          const newArguments = JSON.parse(JSON.stringify(args));
          setArguments([
            ...newArguments.slice(0, i),
            ...newArguments.slice(i + 1),
          ]);
        };
        return (
          <ArgumentEditor
            argument={a}
            setArgument={setArgument}
            removeArgument={removeArgument}
            allTypes={allTypes}
            isLast={isLast}
            index={i}
            service={service}
          />
        );
      })}
    </div>
  );
};

export default ArgumentsEditorList;
