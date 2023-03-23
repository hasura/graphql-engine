import React from 'react';
import styles from './styles.module.scss';

const Player = ({ src }) => {
  const resolvedVideo = require(`@site/static${src}`).default;
  return (
    <div className={`${styles['player']}`}>
      <video autoPlay loop muted playsInline>
        <source src={resolvedVideo} type="video/webm" />
      </video>
    </div>
  );
};

export default Player;
