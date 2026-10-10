import React, { type JSX } from 'react';
import { Flex, Heading, Text } from '@radix-ui/themes';
import Rectangle from './images/Rectangle.svg';
import { LearnMoreLink } from '../components';

type TopicDescriptionProps = {
  title: string;
  imgAlt: string;
  description: React.ReactNode;
  imgElement?: JSX.Element;
  imgUrl?: string;
  learnMoreHref?: string;
};

export const TopicDescription = ({
  title,
  imgUrl,
  imgAlt,
  description,
  learnMoreHref,
  imgElement,
}: TopicDescriptionProps) => {
  return (
    <div>
      <Flex align="center" className="mb-4">
        <img className="mr-2" src={Rectangle} alt="Rectangle" />
        <Heading size="3">{title}</Heading>
      </Flex>
      <div className="px-3.5 pb-9">
        {imgUrl && <img className="w-200" src={imgUrl} alt={imgAlt} />}
        {imgElement ?? null}
      </div>
      <Text as="div" size="2" className="w-8/12">
        {description} {learnMoreHref && <LearnMoreLink href={learnMoreHref} />}
      </Text>
    </div>
  );
};
