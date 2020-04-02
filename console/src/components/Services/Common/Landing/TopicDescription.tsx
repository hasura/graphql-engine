import React from 'react';

type TopicDescriptionProps = {
  title: string;
  imgUrl: string;
  imgAlt: string;
  description: string;
};

const TopicDescription = (props: TopicDescriptionProps) => {
  const Rectangle = require('./images/Rectangle.svg');
  const styles = require('../../RemoteSchema/RemoteSchema.scss');
  const { title, imgUrl, imgAlt, description } = props;
  return (
    <div>
      <div className={styles.subHeaderText}>
        <img className={'img-responsive'} src={Rectangle} alt={'Rectangle'} />
        {title}
      </div>
      <div className={styles.remoteSchemaImg}>
        <img className={'img-responsive'} src={imgUrl} alt={imgAlt} />
      </div>
      <div className={styles.descriptionText + ' ' + styles.wd60}>
        {description}
      </div>
    </div>
  );
};

export default TopicDescription;
