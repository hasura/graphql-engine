import React from 'react';
import { css, keyframes } from 'styled-components';

import { StyledSpinner, StyledSpinnerProps } from './Spinner';

const smallSpinnerSize = 17;
const largeSpinnerSize = 20;

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

export type SpinnerProps = {
  size: string;
};

export const Spinner: React.FC<SpinnerProps & StyledSpinnerProps> = props => {
  const { size } = props;

  const spinnerWidth = size === 'small' ? smallSpinnerSize : largeSpinnerSize;

  const spinnerHeight = size === 'small' ? smallSpinnerSize : largeSpinnerSize;

  return (
    <StyledSpinner
      {...(props as SpinnerProps)}
      height={spinnerHeight}
      width={spinnerWidth}
    >
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
