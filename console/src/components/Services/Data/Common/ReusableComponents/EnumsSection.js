import React from 'react';
import Toggle from 'react-toggle';
import { WarningIcon } from '../../../../Common/WarningSymbol/WarningSymbol';

const EnumsSection = ({ isEnum, toggleEnum, styles, isEnumsCompatible, loading }) => {

  let title = !isEnumsCompatible ? "Only the tables with exactly one or exactly two columns of type text can be marked as enums" : undefined;
  if (loading) {
    title = 'Please wait...';
  }

  const getInconsistencyNote = () => {
    if (!isEnumsCompatible && isEnum) {
      return (
        <div className={`${styles.add_mar_top}`}>
          <WarningIcon customStyle={styles.add_mar_right_small}/>
          <i>This table seems to be in an inconsistent state because enum-incompatible changes have been made to the table schema after marking it as an enum. Please unmark it for everything to work as expected.</i>
        </div>
      )
    }
    return null;
  };

  const getCompatibilityNote = () => {
    return (
      <div>
        <i>The table must be enum compatible before you can add it as an enum. <a href="">Read more.</a></i>
      </div>
    )
  };

  return (
    <div>
      <h4 className={`${styles.subheading_text}`}>
        Mark table as enum
      </h4> 
      <div
        className={`${styles.display_flex} ${styles.add_mar_bottom}`}
        title={title}
        data-toggle="tooltip"
      >
        <span className={styles.add_mar_right_mid}> This table has enum values </span>
        <Toggle
          checked={isEnum}
          icons={false}
          onChange={toggleEnum}
        />
      </div>
      {getCompatibilityNote()}
      {getInconsistencyNote()}
    </div>
  );

};

export default EnumsSection;
