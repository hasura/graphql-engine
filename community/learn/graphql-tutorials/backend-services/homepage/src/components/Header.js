import React from 'react';
import '../styles/styles.scss';
class TopBanner extends React.Component {
  render() {
    const logo = require('../images/logo.svg');
    return (
      <div className={'headerWrapper blueBgColor'}>
        <div className={'container noPadd'}>
          <div className={'logoWrapper'}>
            <a href="https://hasura.io/" target="_blank" rel="noopener noreferrer"><img className={'img-responsive'} src={logo} alt={'Hasura logo'} /></a>
          </div>
        </div>
      </div>
    );
  }
}

export default TopBanner;
