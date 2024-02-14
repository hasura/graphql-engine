import React, { useState } from 'react';
import './DDNBanner.css';

export const DDNBanner = () => {
  const [isVisible, setIsVisible] = useState(true);

  if (!isVisible) return null;

  return (
    <div className="banner">
      <div>
        Hasura DDN is the future of data delivery.&nbsp;<a href="https://hasura.io/docs/3.0/index">Click here for the Hasura DDN docs</a>.
      </div>
      <button className="close-btn" onClick={() => setIsVisible(false)}>
        <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24">
          <path d="M6 18L18 6M6 6l12 12" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round"/>
        </svg>
      </button>
    </div>
  );
}