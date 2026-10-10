import React from 'react';
import { Outlet } from 'react-router';
import styles from './RightContainer.module.scss';

const RightContainer: React.FC<{
  style?: React.CSSProperties;
  children?: React.ReactNode;
}> = ({ children, style }) => {
  return (
    <div className="container-fluid">
      <div className="row">
        <div className={`${styles.main} pl-0 ${styles.padd_top}`} style={style}>
          <div className={`${styles.rightBar}`}>{children}</div>
        </div>
      </div>
    </div>
  );
};

// For use as a route element with nested child routes (renders <Outlet/>
// instead of expecting explicit children, unlike RightContainer itself).
export const RightContainerRoute: React.FC<{
  style?: React.CSSProperties;
}> = ({ style }) => (
  <RightContainer style={style}>
    <Outlet />
  </RightContainer>
);

export default RightContainer;
