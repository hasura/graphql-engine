import React from 'react';

import { ToolTip, Heading, TextLink } from '../../../../UIKit/atoms';
import styles from './Styles.scss';

const editorLabel = 'Kind';
const docsRef =
  'https://docs.hasura.io/1.0/graphql/manual/actions/async-actions.html';

const HandlerEditor = ({ value, onChange, className }) => {
  const setAsynchronous = () => {
    onChange('asynchronous');
  };

  const setSynchronous = () => {
    onChange('synchronous');
  };

  return (
    <div className={className || ''}>
      <Heading type='subHeading' mb='xs'>
        {editorLabel}
        <ToolTip
          message={'Toggle between synchronous vs asynchronous action types'}
          ml='sm'
          mr='20px'
        />
        <TextLink type='moreInfo' href={docsRef} />
      </Heading>
      <div className={styles.display_flex}>
        <label
          className={`${styles.add_mar_right} ${styles.cursorPointer}`}
          onClick={setSynchronous}
        >
          <input
            type='radio'
            checked={value === 'synchronous'}
            readOnly
            className={styles.add_mar_right_small}
          />
          Synchronous
        </label>
        <label
          onClick={setAsynchronous}
          className={`${styles.add_mar_right} ${styles.cursorPointer}`}
        >
          <input
            type='radio'
            readOnly
            checked={value === 'asynchronous'}
            className={styles.add_mar_right_small}
          />
          Asynchronous
        </label>
      </div>
    </div>
  );
};

export default HandlerEditor;
