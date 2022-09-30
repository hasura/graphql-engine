import React from 'react';
import PropTypes from 'prop-types';

const hasuraLogo = require('./images/logo.svg');

import Lottie from 'react-lottie';
import animationData from './loader-logo.json';

const defaultOptions = {
  loop: true,
  autoplay: true,
  animationData: animationData,
  renderer: 'svg',
  rendererSettings: {
    preserveAspectRatio: 'xMidYMid slice'
  }
};
const LottieScreen = () => {
  return (
    <Lottie
      options={defaultOptions}
      height={97}
      width={82}
      isClickToPauseDisabled
    />
  );
};
const LoadingScreen = ({ children, isError }) => {
  const styles = require('./OAuthCallback.scss');
  return (
    <div className={styles.oauth_wrapper}>
      <div className={styles.oauth}>
        <div className={styles.logo}>
          {
            (isError) ? (
              <img src={hasuraLogo} alt={'Hasura Logo'} />
            ) : (
              <LottieScreen />
            )
          }

        </div>
        {children}
      </div>
    </div>
  );
};
LoadingScreen.propTypes = {
  children: PropTypes.func.isRequired,
};
export default LoadingScreen;
