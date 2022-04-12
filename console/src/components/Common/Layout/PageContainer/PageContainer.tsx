import React from 'react';
import Helmet from 'react-helmet';
import styles from '../../Common.scss'

interface PageContainerProps extends React.ComponentProps<'div'> {
  title: string;
  leftContainer: React.ReactNode;
}

const PageContainer: React.FC<PageContainerProps> = ({
  title,
  leftContainer,
  children,
}) => {
  return (
    <>
      <Helmet title={title} />
      <div
        id="left"
        className={`${styles.pageSidebar} bg-white text-gray-800 border-r`}
      >
        {leftContainer}
      </div>
      <div
        id="right"
        className="flex pl-[250px]"
      >
        {children}
      </div>
    </>
  );
};

export default PageContainer;
