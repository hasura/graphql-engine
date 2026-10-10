import React from 'react';
import { Button } from '@hasura/shared/ui';
import styles from '../../Main.module.scss';

type ViewMoreProps = {
  onClickViewMore: () => void;
  readAll: boolean;
};

const ViewMoreOptions: React.FC<ViewMoreProps> = ({
  onClickViewMore,
  readAll,
}) => {
  const buttonText = !readAll
    ? 'View more notifications'
    : 'View older notifications';
  return (
    <Button className={styles.viewMoreNotifications} onClick={onClickViewMore}>
      {buttonText} &rarr;
    </Button>
  );
};

export default ViewMoreOptions;
