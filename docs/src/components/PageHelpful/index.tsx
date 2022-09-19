import React, { Fragment, useState } from 'react';
import { saTrack } from '@site/src/utils/segmentAnalytics';
import styles from './styles.module.scss';
import Hand from '@site/static/img/mascot-hand.png';

// Sleepy time for space between animations / state after submission
function wait(ms = 0) {
  return new Promise((resolve) => {
    setTimeout(resolve, ms);
  });
}

const PageHelpful = () => {
  const [isExpanded, setIsExpanded] = useState(false);
  const [score, setScore] = useState<number>(10);
  const [notes, setNotes] = useState<string>('');
  const [hasResponse, setHasResponse] = useState<boolean>(false);

  function handleNotes(e) {
    setNotes(e.target.value);
  }

  const scores = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

  const recordResponse = async () => {
    // POST request to Cloud Function
    const sendData = async () => {
      var myHeaders = new Headers();
      myHeaders.append('Content-Type', 'application/json');

      var raw = JSON.stringify({
        feedback: {
          isHelpful: score >= 7 ? `👍` : `👎`,
          score,
          notes,
          pageTitle: document.title,
          url: window.location.href,
        },
      });

      var requestOptions = {
        method: 'POST',
        headers: myHeaders,
        body: raw,
        redirect: 'follow',
      };

      fetch('https://us-central1-websitecloud-352908.cloudfunctions.net/docs-feedback', requestOptions)
        .then((response) => response.text())
        .catch((error) => console.log('error', error));
    };

    sendData();

    // For testing, this has been commented out so as not to introduce noise into our analytics
    saTrack('Responded to Did You Find This Page Helpful', {
      label: 'Responded to Did You Find This Page Helpful',
      response: score >= 7 ? 'YES' : 'NO',
      pageUrl: window.location.href,
    });

    // Clear state for next response
    setHasResponse(true);
    setIsExpanded(false);
    setScore(10);
    setNotes('');
    await wait(1000);
    setHasResponse(false);
  };

  return (
    <div
      className={isExpanded ? `${styles.wrapper} ${styles.expanded}` : `${styles.wrapper}`}
      onClick={() => !isExpanded && setIsExpanded(true)}
    >
      {!isExpanded ? (
        <div className={styles.emoji}>{!hasResponse ? <img src={Hand} /> : <p>✅</p>}</div>
      ) : (
        <div className={styles.feedback}>
          <svg
            onClick={() => setIsExpanded(false)}
            xmlns='http://www.w3.org/2000/svg'
            fill='none'
            viewBox='0 0 24 24'
            strokeWidth={1.5}
            stroke='currentColor'
            className={styles.close}
          >
            <path strokeLinecap='round' strokeLinejoin='round' d='M6 18L18 6M6 6l12 12' />
          </svg>
          <div className={styles.form}>
            <h3>Help us with some docs feedback!</h3>
            <p>On a scale of 1 to 10, how helpful would you rate this page?</p>
            <small>1 meaning the page is not helpful at all and 10 meaning you found what you needed quickly.</small>
            <div className={styles.numberRow}>
              {scores.map((scoreItem) => (
                <div
                  key={scoreItem}
                  className={score === scoreItem ? styles.numberActive : ''}
                  onClick={() => setScore(scoreItem)}
                  onKeyDown={() => setScore(scoreItem)}
                  role='button'
                  tabIndex={0}
                >
                  {scoreItem}
                </div>
              ))}
            </div>
            <br />
            <p>
              Any general feedback you'd like to share? We'll take it all...tell us how well we're doing or where can
              improve!
            </p>
            <textarea
              value={notes}
              placeholder='This section is optional ✌️'
              rows='5'
              onChange={(e) => handleNotes(e)}
            />
            <button onClick={() => recordResponse()}>Send it!</button>
          </div>
        </div>
      )}
    </div>
  );
};

export default PageHelpful;
