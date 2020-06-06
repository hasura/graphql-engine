import React from 'react';
import styles from '../../RemoteSchema/RemoteSchema.scss';

const Rectangle = require('./images/Rectangle.svg');

type TopicDescriptionProps = {
  title: string;
  imgUrl: string;
  imgAlt: string;
  description: React.ReactNode;
};

const TopicDescription = (props: TopicDescriptionProps) => {
  const { title, imgUrl, imgAlt, description } = props;
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
        {description}
      </div>
    </div>
  );
};

export default TopicDescription;
