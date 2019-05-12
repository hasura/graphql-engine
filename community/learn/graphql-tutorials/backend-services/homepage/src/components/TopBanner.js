import React from 'react';
import '../styles/styles.scss';

class TopBanner extends React.Component {
  render() {
    const hasuraDumbledore =require('../images/hasura-dumbledore.png');
    return (
      <div className={'gradientBgColor commonSectionWrapper positionRel'}>
        <div className={'container noPadd'}>
          <div className={'topBannerWrapper'}>
            <div className={'col-md-12'}>
              <div className={'col-md-6 col-sm-6 col-xs-12 noPadd'}>
                <div className={'pageHeader'}>
                  Real world GraphQL tutorials for frontend developers with deadlines!
                </div>
                <div className={'whiteLineSeperator'}>
                </div>
                <div className={'sectionDescription'}>
                  You will move from GraphQL basics to production-ready concepts in 2 hours flat.
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
      </div>
    );
  }
}

export default TopBanner;
