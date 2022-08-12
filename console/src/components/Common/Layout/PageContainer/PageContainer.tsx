import React from 'react';
import Helmet from 'react-helmet';
import styles from '../../Common.scss';

interface PageContainerProps extends React.ComponentProps<'div'> {
  helmet: string;
  leftContainer: React.ReactNode;
}

const PageContainer: React.FC<PageContainerProps> = ({
  helmet,
  leftContainer,
  children,
}) => {
  return (
    <>
      <Helmet title={helmet} />
      <div
        className={`${styles.wd20} ${styles.align_left} ${styles.height100}`}
      >
        {leftContainer}
      </div>
      <div className={styles.wd80}>{children}</div>
    </>
  );
};

export default PageContainer;
