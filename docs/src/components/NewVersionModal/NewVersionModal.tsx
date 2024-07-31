import React, { useEffect, useState } from 'react';
import './NewVersionModal.css';
import { useLocalStorage } from 'usehooks-ts';
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import monsta from './monsta.png';

export const NewVersionModal = () => {
  const {
    siteConfig: { customFields },
  } = useDocusaurusContext();

  const [lastClosed, setLastClosed] = useLocalStorage<number | null>(
    `hasuraV${customFields.hasuraVersion}ShowNewVersionLastClosed`,
    null
  );

  const [showModal, setShowModal] = useState(false);

  useEffect(() => {
    const currentTime = Date.now();
    const oneWeek = 7 * 24 * 60 * 60 * 1000; // One week in milliseconds
    // const oneWeek = 60 * 1000; // Temp one minute in milliseconds (for testing)

    if (!lastClosed || (currentTime - lastClosed > oneWeek)) {
      setShowModal(true);
    }
  }, [lastClosed]);

  const handleClose = () => {
    setLastClosed(Date.now());
    setShowModal(false);
  };

  const showHideClassName = showModal ? "new-version-modal display-block" : "new-new-version-modal display-none";

  return (
    <div className={showHideClassName} onClick={handleClose}>
      <div className="modal-main" onClick={e => e.stopPropagation()}>
        <div className="close-icon" onClick={handleClose}>
          <CloseButton/>
        </div>
        <img className="monsta-image" src={monsta} alt="Hasura Logo"/>
        <p className="above-headline">INTRODUCING</p>
        <h1 className="headline">Hasura Data Delivery Network</h1>
        <h5 className="subhead">The next-generation Hasura platform is here!</h5>
        <p className="body-text">Powerful v3 engine. Reimagined console. Multi-team federation. Simplified custom logic
          integration. Enhanced developer experience. And so much more to love!</p>
        <div className="cta-thing"><a target="_blank" rel="noopener noreferrer" href="https://hasura.io/docs/3.0/index/">Check
          out Hasura DDN
          docs</a></div>
      </div>
    </div>
  );
};

const CloseButton = () => {
  return (
    <svg width="32" height="32" viewBox="0 0 32 32" fill="none" xmlns="http://www.w3.org/2000/svg">
      <rect width="32" height="32" rx="16" fill="#F3F4F6"></rect>
      <path d="M21.714 10.286 10.285 21.714m0-11.428 11.429 11.428" stroke="#1F2A37" stroke-width="2"
            stroke-linecap="round" stroke-linejoin="round"></path>
    </svg>
  )
}