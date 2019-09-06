import React from 'react';
import '../styles/styles.scss';
import CommonNavBar from './CommonNavBar';
import {frontendTutorial, backendTutorial, mobileTutorial} from './AllState.js'
class TopBanner extends React.Component {
  render() {
    const logo = 'https://graphql-engine-cdn.hasura.io/learn-hasura/assets/homepage/logo.svg';
    return (
      <div className={'headerWrapper blueBgColor'}>
        <nav className="navbar navbar-default navbarDefault">
          <div className="container">
            <div className="navbar-header navbarHeader">
              <div className={'logoWrapper'}>
                <a href="https://hasura.io/" target="_blank" rel="noopener noreferrer"><img className={'img-responsive'} src={logo} alt={'Hasura logo'} /></a>
              </div>
              <button type="button" className="navbar-toggle collapsed navbarToggle" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
                <span className="sr-only">Toggle navigation</span>
                <span className="icon-bar"></span>
                <span className="icon-bar"></span>
                <span className="icon-bar"></span>
              </button>
            </div>
            <div id="navbar" className="navbar-collapse collapse">
              <ul className="nav navbar-nav navbar-right navBarWrapper">
                <CommonNavBar
                id="frontend"
                navTitle="FRONTEND"
                title="Frontend Tutorials"
                description="2 hour Frontend GraphQL Tutorial Series to teach you your favourite framework."
                commonTutorial = {frontendTutorial}
                />
                <CommonNavBar
                id="mobile"
                navTitle="MOBILE"
                title="Mobile Tutorials"
                description="2 hour mobile GraphQL Tutorial Series to teach you your favourite framework."
                commonTutorial={mobileTutorial}
                />
                <CommonNavBar
                id="backend"
                navTitle="BACKEND"
                title="Backend Tutorials"
                description="2 hour backend GraphQL Tutorial Series to teach you your favourite framework."
                commonTutorial={backendTutorial}
                />
              </ul>
            </div>
          </div>
        </nav>
      </div>
    );
  }
}

export default TopBanner;
