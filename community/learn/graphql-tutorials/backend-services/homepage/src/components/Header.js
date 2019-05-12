import React from 'react';
import '../styles/styles.scss';
class TopBanner extends React.Component {
  render() {
    const logo = require('../images/logo.svg');
    return (
      <div className={'headerWrapper blueBgColor'}>
        <div className={'container noPadd displayFlex'}>
          <div className={'logoWrapper'}>
            <a href="https://hasura.io/" target="_blank" rel="noopener noreferrer"><img className={'img-responsive'} src={logo} alt={'Hasura logo'} /></a>
          </div>
          <ul className="nav navbar-nav navbar-right navBarWrapper">
              <li className="active"><a href="#">MOBILE</a></li>
              <li><a href="#">BACKEND</a></li>
              <li>
              <a href="#">
                <div className="dropdown">
                  <span id="dropdownMenuButton" className="dropdown-toggle" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">FRONTEND</span>
                  <div className="dropdown-menu" aria-labelledby="dropdownMenuButton">
                    <a className="dropdown-item" href="#">Action</a>
                    <a className="dropdown-item" href="#">Another action</a>
                    <a className="dropdown-item" href="#">Something else here</a>
                  </div>
                </div>
              </a>
              </li>
            </ul>
        </div>
      </div>
    );
  }
}

export default TopBanner;
