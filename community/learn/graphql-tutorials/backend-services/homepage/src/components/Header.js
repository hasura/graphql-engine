import React from 'react';
import '../styles/styles.scss';
class TopBanner extends React.Component {
  render() {
    const logo = require('../images/logo.svg');
    const dropPath = require('../images/dropdown-path.svg');
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
                <li className="dropdown">
                  <a id="frontend" className="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">FRONTEND</a>
                  <div aria-labelledby="frontend" className="dropdown-menu dropdownMenu">
                    <div className={'dropdownMenuBgImg'}>
                      <img className={'img-responsive'} src={dropPath} alt={'dropPath'} />
                    </div>
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
                        <li className={'reactBg'}>
                          React
                        </li>
                        <li className={'vueBg'}>
                          Vue
                        </li>
                        <li className={'angularDisableBg displayFlex comingSoonHover'}>
                          Angular <div className={'circle'}></div> <span>Coming soon</span>
                        </li>
                        <li className={'elmDisableBg displayFlex comingSoonHover'}>
                          Elm <div className={'circle'}></div> <span>Coming soon</span>
                        </li>
                      </ul>
                    </div>
                  </div>
                </li>
                <li className="dropdown">
                  <a id="mobile" className="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">MOBILE</a>
                  <div aria-labelledby="mobile" className="dropdown-menu dropdownMenu">
                    <div className={'dropdownMenuBgImg'}>
                      <img className={'img-responsive'} src={dropPath} alt={'dropPath'} />
                    </div>
                    <div className={'col-md-6 col-sm-6 col-xs-12'}>
                      <div className={'menuTitle'}>
                        Mobile Tutorials
                      </div>
                      <div className={'purpleLineSeperator'}>
                      </div>
                      <div className={'sectionDescription'}>
                        2 hour mobile GraphQL Tutorial Series to teach you your favourite framework.
                      </div>
                    </div>
                    <div className={'col-md-6 col-sm-6 col-xs-12'}>
                      <ul className={'dropdownUl'}>
                        <li className={'reactBg'}>
                          React
                        </li>
                        <li className={'iosBg'}>
                          IOS
                        </li>
                        <li className={'androidDisableBg displayFlex comingSoonHover'}>
                          Android <div className={'circle'}></div> <span>Coming soon</span>
                        </li>
                        <li className={'flutterDisableBg displayFlex comingSoonHover'}>
                          Flutter <div className={'circle'}></div> <span>Coming soon</span>
                        </li>
                      </ul>
                    </div>
                  </div>
                </li>
                <li className="dropdown">
                  <a id="backend" className="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">BACKEND</a>
                  <div aria-labelledby="backend" className="dropdown-menu dropdownMenu">
                    <div className={'dropdownMenuBgImg'}>
                      <img className={'img-responsive'} src={dropPath} alt={'dropPath'} />
                    </div>
                    <div className={'col-md-6 col-sm-6 col-xs-12'}>
                      <div className={'menuTitle'}>
                        Backend Tutorials
                      </div>
                      <div className={'purpleLineSeperator'}>
                      </div>
                      <div className={'sectionDescription'}>
                        2 hour backend GraphQL Tutorial Series to teach you your favourite framework.
                      </div>
                    </div>
                    <div className={'col-md-6 col-sm-6 col-xs-12'}>
                      <ul className={'dropdownUl'}>
                        <li className={'hasuraBg'}>
                          Hasura
                        </li>
                        <li className={'postgresDisableBg displayFlex comingSoonHover'}>
                          Vue <div className={'circle'}></div> <span>Coming soon</span>
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
