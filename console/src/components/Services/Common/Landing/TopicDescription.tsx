import React from 'react';
import styles from '../../RemoteSchema/RemoteSchema.scss';
import KnowMoreLink from '../../../Common/KnowMoreLink/KnowMoreLink';

const Rectangle = require('./images/Rectangle.svg');

type TopicDescriptionProps = {
  title: string;
  imgUrl: string;
  imgAlt: string;
  knowMoreHref?: string;
  description: React.ReactNode;
};

const TopicDescription = (props: TopicDescriptionProps) => {
  const { title, imgUrl, imgAlt, description, knowMoreHref } = props;
  return (
    <div>
      <div className={styles.subHeaderText}>
        <img className="img-responsive" src={Rectangle} alt="Rectangle" />
        {title}
      </div>
      <div className={styles.remoteSchemaImg}>
        <img className="img-responsive" src={imgUrl} alt={imgAlt} />
      </div>
      <div className={`${styles.descriptionText} ${styles.wd60}`}>
        {description} {knowMoreHref && <KnowMoreLink href={knowMoreHref} />}
      </div>
    </div>
  );
};

export default TopicDescription;
