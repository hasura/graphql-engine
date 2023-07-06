import React from 'react';
import styles from './styles.module.scss';
const BetaTag: React.FC = (): React.ReactElement => {

  return (
    <div className={styles['beta-tag']}>
      Beta
    </div>
  )
}


export default BetaTag;