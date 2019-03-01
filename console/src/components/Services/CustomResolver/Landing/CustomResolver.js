import React from 'react';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';
import { appPrefix, pageTitle } from '../constants';
import globals from '../../../../Globals';
import Button from '../../Layout/Button/Button';
import PopUp from './PopUp';

class CustomResolver extends React.Component {
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
    const styles = require('../Styles.scss');
    const node = require('./Node.svg');
    const remoteSchema = require('./remote_schema.png');
    const arrowRight = require('./yellow_arrow.svg');
    const glitch = require('./glitch.png');
    const externalLink = require('./external-link.svg');
    const lightGrayArrow = require('./light-gray-arrow.svg');
    const darkGrayArrow = require('./dark-gray-arrow.svg');
    const googleCloud = require('./google_cloud.svg');
    const MicrosoftAzure = require('./Microsoft_Azure_Logo.svg');
    const AWS = require('./AWS.png');
    // const landingImage = require('./schema-stitching-color.png');
    // const landingImage = 'https://storage.googleapis.com/hasura-graphql-engine/console/assets/schema-stitching-diagram.png';

    const { dispatch, migrationMode } = this.props;
    return (
      <div
        className={`${styles.padd_left_remove} ${
          styles.resolverWrapper
        } container-fluid ${styles.padd_top}`}
      >
        <div className={styles.padd_left}>
          <Helmet title={`${pageTitle}s | Hasura`} />
          <div>
            <div className={styles.display_flex}>
              <h2 className={`${styles.headerText} ${styles.addPaddRight} ${styles.inline_block}`}>
                Remote Schemas
              </h2>
              {migrationMode ? (
                <Button
                  data-test="data-create-remote-schemas"
                  color="yellow"
                  size="sm"
                  onClick={e => {
                    e.preventDefault();
                    dispatch(push(`${globals.urlPrefix}${appPrefix}/manage/add`));
                  }}
                >
                  Add
                </Button>
              ) : null}
            </div>
            <hr />
            <div className={styles.subHeaderText}>
              <img className={'img-responsive'} src={arrowRight} alt={'Arrow'} />
              What are Remote Schemas?
            </div>
            <div className={styles.remoteSchemaImg}>
              <img className={'img-responsive'} src={remoteSchema} alt={'Remote Schema'} />
            </div>
            <div className={styles.descriptionText + ' ' + styles.wd60}>
              Remote schemas are the foundation for a set of tools and techniques referred to as schema stitching, a brand new topic in the GraphQL community.  Remote schemas are the foundation for a set of tools and techniques referred to as schema stitching, a brand new topic in the GraphQL community.
            </div>
            <hr className={styles.clear_fix} />
            <div className={styles.subHeaderText}>
              <img className={'img-responsive'} src={arrowRight} alt={'Arrow'} />
              Try it out
            </div>
            <div className={styles.tryOutWrapper}>
              <div className={styles.boxLarge}>
                <div className={styles.logoIcon}>
                  <img className={'img-responsive'} src={glitch} alt={'glitch'} />
                </div>
                <a href={'https://github.com/hasura/graphql-engine/tree/master/community'} target={'_blank'}><button className={styles.default_button}>Try it with Glitch <img className={'img-responsive ' + styles.externalLinkImg} src={externalLink} alt={'externalLink'} /></button></a>
                <div className={styles.instructions + ' ' + styles.instructionsWrapper + ' ' + styles.displayFlex}  onClick = {this.togglePopup.bind(this)}>
                  <span className={styles.instructions + ' ' + styles.displayFlex}><span>Instructions</span><div className={styles.rightArrow}></div></span>
                  { this.state.isPopUp ?
                    (<PopUp onClose = {this.togglePopup.bind(this)} />)
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
          {/*
            <div className={styles.resolverContent}>
              Add pre-CRUD custom business logic like data validation, etc. or also
              fetch data from another GraphQL server by stitching schemas
            </div>
            <div className={styles.resolverImg}>
              <img src={landingImage} />
            </div>
            <div className={styles.commonBtn}>
              <Link
                className={styles.padd_remove_full}
                to={`${appPrefix}/manage/add`}
              >
                <button className={styles.yellow_button}>
                  Add Remote GraphQL schema
                </button>
              </Link>
            </div>
            <div className={styles.readMore}>
              <a
                href="https://docs.hasura.io/1.0/graphql/manual/schema/custom-logic/index.html"
                target="_blank"
                rel="noopener noreferrer"
              >
                Read more
              </a>
            </div>
          */}
        </div>
      </div>
    );
  }
}

const mapStateToProps = state => {
  return {
    migrationMode: state.main.migrationMode,
  };
};

const landingCustomResolverGen = connect =>
  connect(mapStateToProps)(CustomResolver);

export default landingCustomResolverGen;
