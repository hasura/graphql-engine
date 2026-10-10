import React from 'react';
import styles from '../../Main.module.scss';

type ToReadBadgeProps = {
  numberNotifications: number;
  show?: boolean;
};

const ToReadBadge: React.FC<ToReadBadgeProps> = ({
  numberNotifications,
  show,
}) => {
  const showBadge = !show || numberNotifications <= 0 ? styles.hideBadge : '';
  let display = `${numberNotifications}`;
  if (numberNotifications > 20) {
    display = '20+';
  }
  return (
    <div
      className={`flex justify-center items-center ${styles.numBadge} ${showBadge} !-top-3 !-right-2`}
    >
      {display}
    </div>
  );
};

export default ToReadBadge;
