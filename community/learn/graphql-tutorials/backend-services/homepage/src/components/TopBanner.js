import React from 'react';
import '../styles/styles.scss';

class TopBanner extends React.Component {
  render() {
    const hasuraDumbledore = 'https://graphql-engine-cdn.hasura.io/learn-hasura/assets/homepage/hasura-dumbledore.png';
    return (
      <div className={'gradientBgColor commonSectionWrapper positionRel'}>
        <div className={'container noPadd'}>
          <div className={'topBannerWrapper col-md-12'}>
            <div className={'col-md-6 col-sm-6 col-xs-12 noPadd'}>
              <div className={'pageHeader'}>
                Real world GraphQL tutorials for frontend developers with deadlines!
              </div>
              <div className={'whiteLineSeperator'}>
              </div>
              <div className={'sectionDescription'}>
                With these <a href="https://github.com/hasura/graphql-engine/tree/master/community/learn">open-source</a> tutorials, you will move from the basics of GraphQL to building a real-time application in 2 hours
              </div>
            </div>
            <div className={'col-md-6 col-sm-6 col-xs-12 noPadd'}>
              <div className={'topBannerImg'}>
                <img className={'img-responsive'} src={hasuraDumbledore} alt={'Hasura dumbledore'}/>
              </div>
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default TopBanner;
