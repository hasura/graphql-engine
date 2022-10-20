import React from 'react';

import styles from './styles.module.scss';

type NoteProps = {
  type: 'warn';
};
export const Note: React.FC<NoteProps> = ({ type, children }) => {
  return (
    <section className={`${styles.note} ${styles[type]}`}>{children}</section>
  );
};
