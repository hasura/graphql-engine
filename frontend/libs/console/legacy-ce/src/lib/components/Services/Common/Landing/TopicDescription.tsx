import React from 'react';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';
import styles from '../../RemoteSchema/RemoteSchema.module.scss';

import Rectangle from './images/Rectangle.svg';

type TopicDescriptionProps = {
  title: string;
  imgAlt: string;
  description: React.ReactNode;
  imgElement?: JSX.Element;
  imgUrl?: string;
  learnMoreHref?: string;
};

const TopicDescription = (props: TopicDescriptionProps) => {
  const { title, imgUrl, imgAlt, description, learnMoreHref, imgElement } =
    props;
  return (
    <div>
      <div className="flex font-bold text-lg">
        <img className="mr-2" src={Rectangle} alt="Rectangle" />
        {title}
      </div>
      <div className={styles.remoteSchemaImg}>
        {imgUrl && <img src={imgUrl} alt={imgAlt} />}
        {imgElement ?? null}
      </div>
      <div className="text-lg font-normalleading-6 w-8/12">
        {description}
        {learnMoreHref && <LearnMoreLink href={learnMoreHref} />}
      </div>
    </div>
  );
};

export default TopicDescription;
