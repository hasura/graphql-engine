import React from 'react';
import styles from './Styles.scss';
import TypeEditor from './TypeEditor';
import Tooltip from './Tooltip';

const editorLabel = 'Custom Types';
const editorTooltip =
  'You can define GraphQL types for your mutation arguments and mutation returning type';

const Types = ({
  types,
  setTypes,
  className,
  argTypes,
  fieldTypes,
  service,
  removeType,
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
      {types.map((type, i) => {
        if (type.isInbuilt) return null;
        const isLast = i === types.length - 1;
        const setType = t => {
          const newTypes = JSON.parse(JSON.stringify(types));
          newTypes[i] = t;
          setTypes(newTypes);
        };
        const _removeType = () => {
          removeType(i);
        };
        return (
          <TypeEditor
            type={type}
            setType={setType}
            removeType={_removeType}
            isLast={isLast}
            argTypes={argTypes}
            fieldTypes={fieldTypes}
            service={service}
            index={i}
          />
        );
      })}
    </div>
  );
};

export default Types;
