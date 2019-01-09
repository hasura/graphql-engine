import React from 'react';
import '../styles/styles.scss';

class Footer extends React.Component {
  render() {
    const twitter = require('../images/Twitter.svg');
    const discord = require('../images/Discord.svg');
    return (
      <div className={'darkGrayBgColor'}>
        <div className={'container noPadd'}>
          <div className={'footerWrapper'}>
            <div className={'sectionDescription'}>
              Built with <i className="fas fa-heart"></i> by <a href={'http://hasura.io/'} target={'_blank'}>Hasura</a>
            </div>
            <div className={'socialWrapper'}>
              <div className={'socialIcon'}>
                <a href={'https://twitter.com/hasurahq'} target={'_blank'}><img className={'img-responsive'} src={twitter} alt={'twitter'} /></a>
              </div>
              <div className={'socialIcon'}>
                <a href={'https://discord.gg/vBPpJkS'} target={'_blank'}><img className={'img-responsive'} src={discord} alt={'discord'} /></a>
              </div>
            </div>
            <div className={'sectionDescriptionSmall'}>
              © 2018 Hasura Technologies Pvt. Ltd. All rights reserved
            </div>
          </div>
        </div>
      </div>
    );
  }
}
export default Footer;
