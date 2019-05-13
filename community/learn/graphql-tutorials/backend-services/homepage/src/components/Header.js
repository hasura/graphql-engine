import React from 'react';
import '../styles/styles.scss';
import {frontendTutorial, backendTutorial, mobileTutorial} from './AllState.js'
class TopBanner extends React.Component {
  render() {
    const logo = 'https://graphql-engine-cdn.hasura.io/learn-hasura/assets/homepage/logo.svg';
    const dropPath = 'https://graphql-engine-cdn.hasura.io/learn-hasura/assets/homepage/dropdown-path.svg';
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
                  {/* eslint-disable-next-line */}
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
                        {frontendTutorial.map((item, key) => {
                          if(!item.comingSoon) {
                            return (
                              <a href={item.url} target={'_blank'}>
                                <li key={'frontend'+key} className={item.bgClassName}>
                                    {item.name}
                                </li>
                              </a>
                            );
                          } else {
                            return (
                              <li key={'frontend'+key} className={item.disableBgClassName + ' displayFlex comingSoonHover'}>
                                {item.name} <div className={'circle'}></div> <span>Coming soon</span>
                              </li>
                            );
                          }
                        })}
                      </ul>
                    </div>
                  </div>
                </li>
                <li className="dropdown">
                  {/* eslint-disable-next-line */}
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
                        {mobileTutorial.map((item, key) => {
                          if(!item.comingSoon) {
                            return (
                              <a href={item.url} target={'_blank'}>
                                <li key={'mobile'+ key} className={item.bgClassName}>
                                    {item.name}
                                </li>
                              </a>
                            );
                          } else {
                            return (
                              <li key={'mobile'+key} className={item.disableBgClassName + ' displayFlex comingSoonHover'}>
                                {item.name} <div className={'circle'}></div> <span>Coming soon</span>
                              </li>
                            );
                          }
                        })}
                      </ul>
                    </div>
                  </div>
                </li>
                <li className="dropdown">
                  {/* eslint-disable-next-line */}
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
                        {backendTutorial.map((item, key) => {
                          if(!item.comingSoon) {
                            return (
                              <a href={item.url} target={'_blank'}>
                                <li key={'backend'+key} className={item.bgClassName}>
                                    {item.name}
                                </li>
                              </a>
                            );
                          } else {
                            return (
                              <li key={'backend'+key} className={item.disableBgClassName + ' displayFlex comingSoonHover'}>
                                {item.name} <div className={'circle'}></div> <span>Coming soon</span>
                              </li>
                            );
                          }
                        })}
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
