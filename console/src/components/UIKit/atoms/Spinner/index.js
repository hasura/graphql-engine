import React from 'react';

import { StyledSpinner } from './Spinner';

const smallSpinnerSize = 17;
const largeSpinnerSize = 20;

export const Spinner = props => {
  const { size } = props;

  const spinnerWidth = size === 'small' ? smallSpinnerSize : largeSpinnerSize;

  const spinnerHeight = size === 'small' ? smallSpinnerSize : largeSpinnerSize;

  return (
    <StyledSpinner {...props} height={spinnerHeight} width={spinnerWidth}>
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
    </StyledSpinner>
  );
};
