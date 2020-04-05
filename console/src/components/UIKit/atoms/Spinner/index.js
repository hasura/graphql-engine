import React from 'react';
import { css, keyframes } from 'styled-components';

import { StyledSpinner } from './Spinner';

const circleBounceDelay = keyframes`
  0%,
  80%,
  100% {
    transform: scale(0);
  }
  40% {
    transform: scale(1);
  }
`;

const spinnerChildStyles = css`
  width: 100%;
  height: 100%;
  position: absolute;
  left: 0;
  top: 0;

  &:before {
    content: '';
    display: block;
    margin: 0 auto;
    width: 15%;
    height: 15%;
    border-radius: 100%;
    animation: ${circleBounceDelay} 1.2s infinite ease-in-out both;

    background-color: #333;
  }
`;

const smallSpinnerSize = 17;
const largeSpinnerSize = 20;
const extraLargeSpinnerSize = 40;

export const Spinner = props => {
  const { size } = props;

  let spinnerSize;

  switch (size) {
    case 'sm':
      spinnerSize = smallSpinnerSize;
      break;
    case 'lg':
      spinnerSize = largeSpinnerSize;
      break;
    case 'xl':
      spinnerSize = extraLargeSpinnerSize;
      break;
    default:
      spinnerSize = size || smallSpinnerSize;
      break;
  }

  return (
    <StyledSpinner {...props} size={spinnerSize}>
      {Array.from(new Array(12), (_, i) => i).map(i => (
        <div
          key={i}
          css={css`
              ${spinnerChildStyles}
              transform: rotate(${30 * i}deg);

              &:before {
                animation-delay: ${-1.1 + i / 10}s;
              }
            `}
        />
      ))}
    </StyledSpinner>
  );
};
