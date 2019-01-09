import React from 'react';
import '../styles/styles.scss';

import SubscribeFrom from 'react-mailchimp-subscribe';
// TODO: update list
const formProps = {
  action: '//hasura.us13.list-manage.com/subscribe/post?u=9b63e92a98ecdc99732456b0e&amp;id=f5c4f66bcf',
  messages: {
    inputPlaceholder: 'Your Email',
    btnLabel: 'NOTIFY ME',
    sending: 'Subscribing...',
    success: 'Thank you for subscribing!',
    error: 'Invalid email or you have already subscribed for updates'
  },
  styles: {
    sending: {
      fontSize: '15px',
      paddingTop: '10px',
      color: '#ccc'
    },
    success: {
      fontSize: '15px',
      paddingTop: '10px',
      color: '#FFCA27'
    },
    error: {
      fontSize: '15px',
      paddingTop: '10px',
      color: 'red'
    }
  }
};


class TopBanner extends React.Component {
  render() {
    const rightArrow = require('../images/right-arrow.svg');
    const mobile = require('../images/smartphone.svg');
    const graphqlLogo =require('../images/GraphQL_Logo.png');
    const hasuraDumbledore =require('../images/hasura-dumbledore.png');
    const group =require('../images/Group.svg');
    return (
      <div className={'gradientBgColor commonSectionWrapper positionRel commonSectionWrapperBot'}>
        <div className={'container noPadd'}>
          <div className={'topBannerWrapper'}>
            <div className={'col-md-6 col-sm-6 col-xs-12 noPadd'}>
              <div className={'pageHeader'}>
                Real world GraphQL tutorials for frontend developers with deadlines!
              </div>
              <div className={'sectionDescription'}>
                You will move from GraphQL basics to production-ready concepts in 2 hours flat.
              </div>
              <div className={'notifyWrapper'}>
                <div className={'sectionDescription notifyHeader'}>
                  Subscribe for updates
                </div>
                <SubscribeFrom className={"notifyForm"} {...formProps}/>
              </div>
            </div>
            <div className={'col-md-6 col-sm-6 col-xs-12 noPadd'}>
              <div className={'topBannerImg'}>
                <img className={'img-responsive'} src={hasuraDumbledore} alt={'Hasura dumbledore'}/>
              </div>
            </div>
          </div>
          <div className={'positionWrapper iconWrapper boxShadow'}>
            <div className={'sectionSubHeader'}>
              This tutorial will teach you what GraphQL is and how to integrate GraphQL APIs with your favourite web/mobile framework in a real world scenario
            </div>
            <div className={'iconListWrapper wd100'}>
              <div className={'col-md-4 col-sm-4 col-xs-12'}>
                <div className={'iconList'}>
                  <div className={'scenarioImg'}>
                    <img className={'img-responsive'} src={mobile} alt={'Mobile'}/>
                  </div>
                  <div className={'listHeader'}>
                    SCENARIO 1
                  </div>
                  <div className={'sectionDescription'}>
                    The UI of your realtime app is ready, but the app isn’t integrated with the backend
                  </div>
                </div>
              </div>
              <div className={'col-md-4 col-sm-4 col-xs-12'}>
                <div className={'iconList'}>
                  <div className={'scenarioImg'}>
                    <img className={'img-responsive'} src={graphqlLogo} alt={'GraphQL logo'}/>
                  </div>
                  <div className={'listHeader'}>
                    SCENARIO 2
                  </div>
                  <div className={'sectionDescription'}>
                    Your backend team has given you an auth system and a GraphQL API
                  </div>
                </div>
              </div>
              <div className={'col-md-4 col-sm-4 col-xs-12'}>
                <div className={'iconList'}>
                  <div className={'scenarioImg'}>
                    <img className={'img-responsive'} src={group} alt={'group'}/>
                  </div>
                  <div className={'listHeader'}>
                    SCENARIO 3
                  </div>
                  <div className={'sectionDescription'}>
                    Your task is to integrate the APIs to your app, ensure best practices and be production ready
                  </div>
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
