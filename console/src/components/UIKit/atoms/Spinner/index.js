import React from 'react';

import { SpinnerStyles } from './Spinner.style';

const Spinner = props => {
  return (
    <SpinnerStyles {...props}>
      <div className={'sk_circle1' + ' ' + 'sk_child'} />
      <div className={'sk_circle2' + ' ' + 'sk_child'} />
      <div className={'sk_circle3' + ' ' + 'sk_child'} />
      <div className={'sk_circle4' + ' ' + 'sk_child'} />
      <div className={'sk_circle5' + ' ' + 'sk_child'} />
      <div className={'sk_circle6' + ' ' + 'sk_child'} />
      <div className={'sk_circle7' + ' ' + 'sk_child'} />
      <div className={'sk_circle8' + ' ' + 'sk_child'} />
      <div className={'sk_circle9' + ' ' + 'sk_child'} />
      <div className={'sk_circle10' + ' ' + 'sk_child'} />
      <div className={'sk_circle11' + ' ' + 'sk_child'} />
      <div className={'sk_circle12' + ' ' + 'sk_child'} />
    </SpinnerStyles>
  );
};

// Default props for Spinner ******* //

Spinner.defaultProps = {
  width: 40,
  height: 40,
};

// ********************************** //

export default Spinner;
