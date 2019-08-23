import React from 'react';
import PropTypes from 'prop-types';
import PopUp from './PopUp';

class TryItOut extends React.Component {
  constructor() {
    super();
    this.state = {
      isPopUp: false,
    };
  }
  togglePopup() {
    this.setState({ isPopUp: !this.state.isPopUp });
  }
  render() {
    const Rectangle = require('./images/Rectangle.svg');
    const styles = require('../../CustomResolver/CustomResolver.scss');
    const glitch = require('./images/glitch.png');
    const googleCloud = require('./images/google_cloud.svg');
    const MicrosoftAzure = require('./images/Microsoft_Azure_Logo.svg');
    const AWS = require('./images/AWS.png');
    const externalLink = require('./images/external-link.svg');
    // const { title, imgUrl, imgAlt,  description} = this.props;
    const commonStyle = this.props.isAvailable
      ? styles.instructionsWrapper
      : styles.instructionsWrapperPos;
    return (
      <div>
        <div className={styles.subHeaderText}>
          <img className={'img-responsive'} src={Rectangle} alt={'Rectangle'} />
          Try it out
        </div>
        <div className={styles.tryOutWrapper}>
          <div className={styles.boxLarge}>
            <div className={styles.logoIcon}>
              <img className={'img-responsive'} src={glitch} alt={'glitch'} />
            </div>
            <a
              href={this.props.glitchLink}
              target="_blank"
              rel="noopener noreferrer"
            >
              <button className={styles.default_button}>
                Try it with Glitch{' '}
                <img
                  className={'img-responsive ' + styles.externalLinkImg}
                  src={externalLink}
                  alt={'externalLink'}
                />
              </button>
            </a>
            <div className={styles.displayFlex + ' ' + commonStyle}>
              <span
                onClick={this.togglePopup.bind(this)}
                className={styles.instructions + ' ' + styles.displayFlex}
              >
                <span>Instructions</span>
                <div className={styles.rightArrow} />
              </span>
              {this.state.isPopUp ? (
                <PopUp
                  onClose={this.togglePopup.bind(this)}
                  service={this.props.service}
                  title={this.props.title}
                  queryDefinition={this.props.queryDefinition}
                  footerDescription={this.props.footerDescription}
                  isAvailable={this.props.isAvailable}
                />
              ) : null}
            </div>
          </div>
          <div className={styles.boxSmallWrapper}>
            <a
              href={this.props.googleCloudLink}
              target={'_blank'}
              rel="noopener noreferrer"
              title={'Google Cloud'}
            >
              <div className={styles.boxSmall}>
                <div className={styles.logoIcon}>
                  <img
                    className={'img-responsive'}
                    src={googleCloud}
                    alt={'Google Cloud'}
                  />
                </div>
              </div>
            </a>
            <a
              href={this.props.MicrosoftAzureLink}
              target={'_blank'}
              rel="noopener noreferrer"
              title={'Microsoft Azure'}
            >
              <div className={styles.boxSmall}>
                <div className={styles.logoIcon}>
                  <img
                    className={'img-responsive'}
                    src={MicrosoftAzure}
                    alt={'Microsoft Azure'}
                  />
                </div>
              </div>
            </a>
            <a
              href={this.props.awsLink}
              target={'_blank'}
              rel="noopener noreferrer"
              title={'AWS'}
            >
              <div className={styles.boxSmall}>
                <div className={styles.logoIcon}>
                  <img
                    className={'img-responsive ' + styles.imgAws}
                    src={AWS}
                    alt={'AWS'}
                  />
                </div>
              </div>
            </a>
            <div className={styles.instructions}>
              <a
                href={this.props.adMoreLink}
                target="_blank"
                rel="noopener noreferrer"
              >
                <span>And many more</span> <div className={styles.rightArrow} />
              </a>
            </div>
          </div>
        </div>
      </div>
    );
  }
}
TryItOut.propTypes = {
  service: PropTypes.string.isRequired,
  queryDefinition: PropTypes.string.isRequired,
  glitchLink: PropTypes.string.isRequired,
  googleCloudLink: PropTypes.string.isRequired,
  MicrosoftAzureLink: PropTypes.string.isRequired,
  awsLink: PropTypes.string.isRequired,
  adMoreLink: PropTypes.string.isRequired,
  // imgUrl: PropTypes.string.isRequired,
  // imgAlt: PropTypes.string.isRequired,
  // description: PropTypes.string.isRequired,
};
export default TryItOut;
