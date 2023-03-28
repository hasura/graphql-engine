import React from 'react';
import styles from './styles.module.scss';

type PlayerProps = {
  src: string;
  autoPlay?: boolean;
  loop?: boolean;
  muted?: boolean;
  playsInline?: boolean;
  showControls?: boolean;
};

const Player = (props: PlayerProps) => {
  const { src, autoPlay = true, loop = true, muted = true, playsInline = true, showControls = false } = props;
  const resolvedVideo = require(`@site/static${src}`).default;
  return (
    <div className={`${styles['player']}`}>
      <video autoPlay={autoPlay} loop={loop} muted={muted} playsInline={playsInline} controls={showControls}>
        <source src={resolvedVideo} type="video/webm" />
      </video>
    </div>
  );
};

export default Player;
