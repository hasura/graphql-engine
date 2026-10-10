import styles from './LoadingScreen.module.scss';
import { Link, To } from 'react-router';
import { Button, IndicatorCard } from '../../components';
import { Flex, Text } from '@radix-ui/themes';
import { ReactNode } from 'react';

type LoadingScreenTitleProps = {
  title: string;
};

export const LoadingScreenTitle = ({ title }: LoadingScreenTitleProps) => {
  return <div className={styles['validating_wrapper']}>{title}</div>;
};

type LoadingScreenErrorProps = {
  title: string;
  message?: ReactNode;
  link?: To;
  linkText?: string;
};

export const LoadingScreenError = ({
  title,
  message,
  link,
  linkText = 'Go Back',
}: LoadingScreenErrorProps) => {
  const getErrorDescription = message ? (
    <div className="py-4">
      <Text as="div" size="3" color="gray" align="center">
        {message}
      </Text>
    </div>
  ) : null;

  const getBackButton = link ? (
    <Flex justify="center">
      <Link to="/login">
        <Button type="button" mode="destructive">
          {linkText}
        </Button>
      </Link>
    </Flex>
  ) : null;

  return (
    <Flex direction="column" align="center" justify="center">
      <div>
        <IndicatorCard status="negative" showIcon>
          {title}
        </IndicatorCard>
        {getErrorDescription}
        {getBackButton}
      </div>
    </Flex>
  );
};
