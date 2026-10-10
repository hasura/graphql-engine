import React from 'react';
import Logo from './images/Logo';
import { LottieSvg } from 'lottie-react';
import animationData from './loader-logo.json';
import { useAppearance } from '../../theme';
import { Flex } from '@radix-ui/themes';

const LottieScreen = () => {
  return (
    <LottieSvg
      src={animationData}
      loop
      autoplay
      rendererSettings={{
        preserveAspectRatio: 'xMidYMid slice',
      }}
      style={{ width: 82, height: 97, overflow: 'hidden', margin: '0 auto' }}
      role="img"
      aria-label="Loading"
    />
  );
};

export const LoadingScreen = ({
  children,
  isError,
}: {
  isError?: boolean;
  children: React.ReactNode;
}) => {
  const { appearance } = useAppearance();

  return (
    <Flex align="center" justify="center" className="w-full h-screen">
      <Flex
        direction="column"
        justify="center"
        align="center"
        gap="4"
        className={'max-w-[500px]'}
      >
        <div>
          {isError ? (
            <Logo
              className="inline-block"
              fill={appearance === 'dark' ? 'white' : '#717780'}
            />
          ) : (
            <LottieScreen />
          )}
        </div>
        {children}
      </Flex>
    </Flex>
  );
};
