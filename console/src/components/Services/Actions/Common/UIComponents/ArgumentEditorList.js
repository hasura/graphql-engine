import React from 'react';
import styles from './Styles.scss';
import Tooltip from './Tooltip';
import ArgumentEditor from './ArgumentEditor';

const editorLabel = 'Input Arguments';
const editorTooltip = 'These will be the arguments of the mutation';

const ArgumentsEditorList = ({
  className,
  types,
  args,
  setArguments,
  setTypes,
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
          if (isLast && !!arg.name && !!arg.type) {
            newArguments.push({ name: '', type: '', description: '' });
          }
          setArguments(newArguments);
        };
        return (
          <ArgumentEditor
            argument={a}
            setArgument={setArgument}
            types={types}
            setTypes={setTypes}
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
