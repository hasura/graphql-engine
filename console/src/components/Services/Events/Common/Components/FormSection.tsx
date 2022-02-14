import React from 'react';
import styles from '../../Events.scss';
import Tooltip from '../../../../Common/Tooltip/Tooltip';

interface Props extends React.ComponentProps<React.FC> {
  heading: string;
  id: string;
  tooltip?: string;
}

const FormSection: React.FC<Props> = ({ children, id, tooltip, heading }) => {
  return (
    <div className={styles.add_mar_bottom}>
      <h2
        className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
      >
        {heading}
        {tooltip && (
          <Tooltip
            id={id}
            message={tooltip}
            className={styles.add_mar_left_mid}
          />
        )}
      </h2>
      {children}
      <hr className="my-md" />
    </div>
  );
};

export default FormSection;
