import React from 'react';
import '../styles/styles.scss';
class TopBanner extends React.Component {
  render() {
    const logo = require('../images/logo.svg');
    return (
      <div className={'headerWrapper blueBgColor'}>
        <nav className="navbar navbar-default navbarDefault">
          <div className="container">
            <div className="navbar-header">
              <button type="button" className="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
                <span className="sr-only">Toggle navigation</span>
                <span className="icon-bar"></span>
                <span className="icon-bar"></span>
                <span className="icon-bar"></span>
              </button>
              <div className={'logoWrapper'}>
                <a href="https://hasura.io/" target="_blank" rel="noopener noreferrer"><img className={'img-responsive'} src={logo} alt={'Hasura logo'} /></a>
              </div>
            </div>
            <div id="navbar" className="navbar-collapse collapse">
              <ul className="nav navbar-nav navbar-right navBarWrapper">
                <li className="active"><a href="https://www.google.com/">MOBILE</a></li>
                <li><a href="https://www.google.com/">BACKEND</a></li>
                <li className="dropdown">
                  <a className="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">FRONTEND</a>
                  <div className="dropdown-menu dropdownMenu">
                    <div className={'col-md-6 col-sm-6 col-xs-12'}>
                      <div className={'menuTitle'}>
                      Frontend Tutorials
                      </div>
                      <div className={'purpleLineSeperator'}>
                      </div>
                      <div className={'sectionDescription'}>
                        2 hour Frontend GraphQL Tutorial Series to teach you your favourite framework.
                      </div>
                    </div>
                    <div className={'col-md-6 col-sm-6 col-xs-12'}>
                      <ul className={'dropdownUl'}>
                        <li className={'react'}>
                          React
                        </li>
                        <li className={'vue'}>
                          Vue
                        </li>
                        <li className={'angular'}>
                          Angular
                        </li>
                        <li className={'elm displayFlex comingSoonHover'}>
                          Elm <div className={'circle'}></div> Coming soon
                        </li>
                      </ul>
                    </div>
                  </div>
                </li>
              </ul>
            </div>
          </div>
        </nav>
      </div>
    );
  }
}

export default TopBanner;
