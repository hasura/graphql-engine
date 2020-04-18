import React from 'react';
import PropTypes from 'prop-types';

import { Heading, Box } from '../../../UIKit/atoms';
import styles from '../../RemoteSchema/RemoteSchema.scss';
import Rectangle from './images/Rectangle.svg';

const TopicDescription = ({ title, imgUrl, imgAlt, description }) => (
  <div>
    <div className={styles.subHeaderText}>
      <img className={'img-responsive'} src={Rectangle} alt={'Rectangle'} />
      <Heading as="h4" display="inline-block" fontSize="18px" fontWeight={6}>
        {title}
      </Heading>
    </div>
    <div className={styles.remoteSchemaImg}>
      <img className={'img-responsive'} src={imgUrl} alt={imgAlt} />
    </div>
    <Box width="60%" fontSize="15px" lineHeight="1.2" fontWeight="normal">
      {description}
    </Box>
  </div>
);

TopicDescription.propTypes = {
  title: PropTypes.string.isRequired,
  imgUrl: PropTypes.string.isRequired,
  imgAlt: PropTypes.string.isRequired,
  description: PropTypes.string.isRequired,
};

export default TopicDescription;
