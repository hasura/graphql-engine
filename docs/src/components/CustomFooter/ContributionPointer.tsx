import React, { useEffect, useState } from 'react';
import Link from '@docusaurus/Link';
import styles from './styles.module.scss';

const ContributionPointer = ({ element }) => {
  const [isHover, setIsHover] = useState(false);
  useEffect(() => {
    if (!element) return;
    element.addEventListener('mouseenter', () => {
      setIsHover(true);
    });
    element.addEventListener('mouseleave', () => {
      setIsHover(false);
    });
  }, [element]);

  return (
    <Link
      className={`${styles['contribution-pointer']} ${isHover ? styles['contribution-hover'] : ''}`}
      href="https://hasura.io/docs/wiki/contributions/"
      rel="noopener noreferrer"
    >
      <div className={styles['contribution-pointer__text']}>
        <span>Have questions? Check out our contribution guide!</span>
      </div>
    </Link>
  );
};

export default ContributionPointer;
