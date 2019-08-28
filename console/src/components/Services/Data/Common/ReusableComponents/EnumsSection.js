import React from 'react';
import Toggle from 'react-toggle';

const EnumsSection = ({ isEnum, toggleEnum, styles, isEnumsCompatible, loading }) => {

  let title = !isEnumsCompatible ? "Only the tables with exactly one or exactly two columns of type text can be marked as enums" : undefined;
  if (loading) {
    title = 'Please wait...';
  }
  return (
    <div>
      <h4 className={`${styles.subheading_text}`}>
        Mark table as enum
      </h4> 
      <label className={`${styles.display_flex}`}>
        <span className={styles.add_mar_right_mid}> This table has enum values </span>
        <Toggle
          checked={isEnum}
          icons={false}
          onChange={toggleEnum}
          disabled={!isEnumsCompatible || loading}
          title={title}
        />
      </label>
    </div>
  )

};

export default EnumsSection;
