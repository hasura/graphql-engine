import React from 'react';
import PropTypes from 'prop-types';

class TopicDescription extends React.Component {
  render() {
    const Rectangle = require('./images/Rectangle.svg');
    const styles = require('../../RemoteSchema/RemoteSchema.scss');
    const { title, imgUrl, imgAlt, description } = this.props;
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
  }
}
TopicDescription.propTypes = {
  title: PropTypes.string.isRequired,
  imgUrl: PropTypes.string.isRequired,
  imgAlt: PropTypes.string.isRequired,
  description: PropTypes.string.isRequired,
};
export default TopicDescription;
