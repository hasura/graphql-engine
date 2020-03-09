import React from 'react';

import { SpinnerStyles } from './Spinner.style';

export const Spinner = props => {
  const { size } = props;

  // Spinner size (width & height ~ px values) depends upon the size prop received from parent <Button />

  const spinnerWidth = size === 'small' ? 17 : 20;

  const spinnerHeight = size === 'small' ? 17 : 20;

  // ****************************** //

  return (
    <SpinnerStyles {...props} height={spinnerHeight} width={spinnerWidth}>
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

Spinner.defaultProps = {
  ml: '12px',
};
