import React, { useEffect, useState } from 'react';
import styles from './NewVersionModal.module.css';
import { useLocalStorage } from 'usehooks-ts';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
// @ts-ignore
import monsta from './monsta.png';

export const NewVersionModal = () => {
  const {
    siteConfig: { customFields },
  } = useDocusaurusContext();

  const [isDismissed, setDismissed] = useLocalStorage<boolean>(`hasuraV${customFields.hasuraVersion}Dismissed`, false);

  const [showModal, setShowModal] = useState(false);

  useEffect(() => {
    if (!isDismissed) {
      setShowModal(true);
    }
  }, [isDismissed]);

  const handleClose = () => {
    setDismissed(true);
    setShowModal(false);
  };

  return (
    <div className={showModal ? styles.newVersionModal : styles.displayNone} onClick={handleClose}>
      <div className={styles.modalMain} onClick={e => e.stopPropagation()}>
        <div className={styles.closeIcon} onClick={handleClose}>
          <CloseButton />
        </div>
        <img className={styles.monstaImage} src={monsta} alt="Hasura Logo" />
        <p className={styles.aboveHeadline}>INTRODUCING</p>
        <h1 className={styles.headline}>Hasura Data Delivery Network</h1>
        <h5 className={styles.subhead}>The next-generation Hasura platform is here!</h5>
        <p className={styles.bodyText}>
          Powerful v3 engine. Reimagined console. Multi-team federation. Simplified custom logic integration. Enhanced
          developer experience. And so much more to love!
        </p>
        <div className={styles.ctaThing}>
          <a target="_blank" rel="noopener noreferrer" href="https://hasura.io/docs/3.0/index/" onClick={handleClose}>
            Check out Hasura DDN docs
          </a>
        </div>
      </div>
    </div>
  );
};

const CloseButton = () => {
  return (
    <svg width="32" height="32" viewBox="0 0 32 32" fill="none" xmlns="http://www.w3.org/2000/svg">
      <rect width="32" height="32" rx="16" fill="#F3F4F6"></rect>
      <path
        d="M21.714 10.286 10.285 21.714m0-11.428 11.429 11.428"
        stroke="#1F2A37"
        strokeWidth="2"
        strokeLinecap="round"
        strokeLinejoin="round"
      ></path>
    </svg>
  );
};
