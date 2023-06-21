import React, { ReactNode, useState } from 'react';
import { saTrack } from '@site/src/utils/segmentAnalytics';
import styles from './styles.module.scss';
export const Feedback = ({ metadata }: { metadata: any }) => {
  const [rating, setRating] = useState<1 | 2 | 3 | 4 | 5 | null>(null);
  const [notes, setNotes] = useState<string | null>(null);
  const [errorText, setErrorText] = useState<string | null>(null);
  const [hoveredScore, setHoveredScore] = useState<Number | null>(null);
  const [textAreaLabel, setTextAreaLabel] = useState<ReactNode | null>(null);
  const [textAreaPlaceholder, setTextAreaPlaceholder] = useState<string>('This section is optional ✌️');
  const [isSubmitSuccess, setIsSubmitSuccess] = useState<boolean>(false);

  const submitDisabled = rating === null || (rating < 4 && (notes === null || notes === ''));

  const scores: (1 | 2 | 3 | 4 | 5)[] = [1, 2, 3, 4, 5];

  const handleSubmit = async () => {
    if (rating === null) {
      setErrorText('Please select a score.');
      return;
    }

    if (rating < 4 && notes === null) {
      setErrorText(
        "Because this doc wasn't up to scratch please provide us with some feedback of where we can improve."
      );
      return;
    }

    const sendData = async () => {
      const myHeaders = new Headers();
      myHeaders.append('Content-Type', 'application/json');

      const raw = JSON.stringify({
        feedback: {
          isHelpful: rating >= 4 ? `👍` : `👎`,
          score: rating,
          notes,
          pageTitle: document.title,
          url: window.location.href,
        },
      });

      const requestOptions = {
        method: 'POST',
        headers: myHeaders,
        body: raw,
        redirect: 'follow',
      };

      fetch('https://us-central1-websitecloud-352908.cloudfunctions.net/docs-feedback', requestOptions)
        .then(response => response.text())
        .catch(error => console.error('error', error));
    };

    if (!window.location.hostname.includes('hasura.io')) {
      alert(
        'Hey! We like that you like our docs and chose to use them 🎉\n\nHowever, you might want to remove the feedback component or modify the route you hit, lest you want us reading what people think of your site ✌️'
      );
      setRating(null);
      setNotes(null);
      setIsSubmitSuccess(true);
      return;
    }

    sendData()
      .then(() => {
        saTrack('Responded to Did You Find This Page Helpful', {
          label: 'Responded to Did You Find This Page Helpful',
          response: rating >= 4 ? 'YES' : 'NO',
          pageUrl: window.location.href,
        });
        setRating(null);
        setNotes(null);
        setIsSubmitSuccess(true);
      })
      .catch(e => {
        console.error(e);
      });

    return;
  };

  const handleScoreClick = (scoreItem: 1 | 2 | 3 | 4 | 5) => {
    if (scoreItem === rating) {
      setRating(null);
      setErrorText(null);
      setHoveredScore(null);
      return;
    }
    setErrorText(null);
    setRating(scoreItem);
    if (scoreItem < 4) {
      setTextAreaLabel(
        <>
          <p>What can we do to improve it? Please be as detailed as you like.</p>
          <p>Real human beings read every single review.</p>
        </>
      );
      setTextAreaPlaceholder('This section is required... how can we do better? ✍️');
    }
    if (scoreItem >= 4) {
      setTextAreaLabel(
        <>
          <p>Any general feedback you'd like to add?</p>
          <p>We'll take it all... tell us how well we're doing or where we can improve.</p>
          <p>Real human beings read every single review.</p>
        </>
      );
      setTextAreaPlaceholder('This section is optional ✌️');
    }
  };

  // Do not show on Intro page
  if (metadata.source === '@site/docs/index.mdx') {
    return null;
  }

  return (
    <div className={styles.feedback} id={'feedback'}>
      <div className={styles.form}>
        <div className={styles.topSection}>
          <h3>What did you think of this doc?</h3>
          {isSubmitSuccess ? (
            <div className={styles.successMessage}>
              <p>Thanks for your feedback.</p>
              {rating >= 3 ? (
                <p>Feel free to review as many docs pages as you like!</p>
              ) : (
                <p>
                  If you need help with the issue that led to this low score, you can create a{' '}
                  <a
                    href="https://github.com/hasura/graphql-engine/issues/new/choose"
                    target="_blank"
                    rel="noopener noreferrer"
                  >
                    GitHub issue
                  </a>{' '}
                  if you think this is a bug, or check out our{' '}
                  <a href="https://hasura.io/discord" target="_blank" rel="noopener noreferrer">
                    Discord server
                  </a>
                  , where Hasurians and community users are ready to engage.
                </p>
              )}
            </div>
          ) : (
            <div className={styles.numberRow}>
              {scores.map((star, index) => (
                <div
                  className={styles.star}
                  key={star}
                  onClick={() => handleScoreClick(star)}
                  onMouseEnter={() => setHoveredScore(index + 1)}
                  onMouseLeave={() => setHoveredScore(-1)}
                >
                  {rating >= star ? (
                    <svg width="36" height="36" viewBox="0 0 24 24">
                      <path
                        fill="#ffc107"
                        d="M12,17.27L18.18,21L16.54,13.97L22,9.24L14.81,8.62L12,2L9.19,8.62L2,9.24L7.45,13.97L5.82,21L12,17.27Z"
                      />
                    </svg>
                  ) : (
                    <svg width="36" height="36" viewBox="0 0 24 24">
                      <path
                        fill={hoveredScore > index ? '#ffc107' : '#B1BCC7'}
                        d="M12,17.27L18.18,21L16.54,13.97L22,9.24L14.81,8.62L12,2L9.19,8.62L2,9.24L7.45,13.97L5.82,21L12,17.27Z"
                      />
                    </svg>
                  )}
                </div>
              ))}
            </div>
          )}
        </div>
        <div style={rating ? { display: 'block' } : { display: 'none' }}>
          <div className={styles.textAreaLabel}>{textAreaLabel}</div>
          <textarea
            className={styles.textarea}
            value={notes ?? ''}
            placeholder={textAreaPlaceholder ?? ''}
            rows={5}
            onChange={e => setNotes(e.target.value)}
          />
          <div className={styles.errorAndButton}>
            <p className={styles.errorText}>{errorText}</p>
            <div className={styles.buttonContainer}>
              <button className={submitDisabled ? styles.buttonDisabled : ''} onClick={() => handleSubmit()}>
                Send your review!
              </button>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};
