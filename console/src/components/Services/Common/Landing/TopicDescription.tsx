import React from 'react';
import styles from '../../RemoteSchema/RemoteSchema.scss';
import KnowMoreLink from '../../../Common/KnowMoreLink/KnowMoreLink';

const Rectangle = require('./images/Rectangle.svg');

type TopicDescriptionProps = {
  title: string;
  imgAlt: string;
  description: React.ReactNode;
  imgElement?: JSX.Element;
  imgUrl?: string;
  knowMoreHref?: string;
};

const TopicDescription = (props: TopicDescriptionProps) => {
  const {
    title,
    imgUrl,
    imgAlt,
    description,
    knowMoreHref,
    imgElement,
  } = props;
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
        {description} {knowMoreHref && <KnowMoreLink href={knowMoreHref} />}
      </div>
    </div>
  );
};

export default TopicDescription;
