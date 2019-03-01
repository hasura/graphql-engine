import React from 'react';
import PropTypes from 'prop-types';
import PopUp from '../CustomResolver/Landing/PopUp';
class TryItOut extends React.Component {
  constructor() {
    super();
    this.state = {
      isPopUp: false,
    };
  }
  togglePopup() {
    this.setState({isPopUp : !this.state.isPopUp});
  }
  render() {
    const Rectangle = require('../CustomResolver/Landing/Rectangle.svg');
    const styles = require('../CustomResolver/Styles.scss');
    const glitch = require('../CustomResolver/Landing/glitch.png');
    const googleCloud = require('../CustomResolver/Landing/google_cloud.svg');
    const MicrosoftAzure = require('../CustomResolver/Landing/Microsoft_Azure_Logo.svg');
    const AWS = require('../CustomResolver/Landing/AWS.png');
    const externalLink = require('../CustomResolver/Landing/external-link.svg');
    // const { title, imgUrl, imgAlt,  description} = this.props;
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
            <a href={'https://github.com/hasura/graphql-engine/tree/master/community'} target={'_blank'}><button className={styles.default_button}>Try it with Glitch <img className={'img-responsive ' + styles.externalLinkImg} src={externalLink} alt={'externalLink'} /></button></a>
            <div className={styles.instructionsWrapper + ' ' + styles.displayFlex}>
              <span   onClick = {this.togglePopup.bind(this)} className={styles.instructions + ' ' + styles.displayFlex}><span>Instructions</span><div className={styles.rightArrow}></div></span>
              { this.state.isPopUp ?
                (<PopUp
                  onClose = {this.togglePopup.bind(this)}
                  service = {this.props.service}
                  title= "Steps to deploy an example GraphQL service to Glitch"
                  queryDefinition= {this.props.queryDefinition}
                  footerDescription= "You just added a remote schema and queried it!"/>)
                : null }
            </div>
          </div>
          <div className={styles.boxSmallWrapper}>
            <a href={'https://github.com/hasura/graphql-engine/tree/master/community'} target={'_blank'}>
              <div className={styles.boxSmall}>
                <div className={styles.logoIcon}>
                  <img className={'img-responsive'} src={googleCloud} alt={'googleCloud'} />
                </div>
              </div>
            </a>
            <a href={'https://github.com/hasura/graphql-engine/tree/master/community'} target={'_blank'}>
              <div className={styles.boxSmall}>
                <div className={styles.logoIcon}>
                  <img className={'img-responsive'} src={MicrosoftAzure} alt={'Microsoft Azure'} />
                </div>
              </div>
            </a>
            <a href={'https://github.com/hasura/graphql-engine/tree/master/community'} target={'_blank'}>
              <div className={styles.boxSmall}>
                <div className={styles.logoIcon}>
                  <img className={'img-responsive ' + styles.imgAws} src={AWS} alt={'AWS'} />
                </div>
              </div>
            </a>
            <div className={styles.instructions}>
              <span>And many more</span> <div className={styles.rightArrow}></div>
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
  // imgUrl: PropTypes.string.isRequired,
  // imgAlt: PropTypes.string.isRequired,
  // description: PropTypes.string.isRequired,
};
export default TryItOut;
