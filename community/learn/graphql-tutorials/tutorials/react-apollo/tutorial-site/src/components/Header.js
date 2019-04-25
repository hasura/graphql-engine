import React from 'react';
import { StaticQuery, graphql } from 'gatsby';
import GitHubButton from 'react-github-btn'
import './styles.css';

import Sidebar from "./sidebar";

const Header = ({location}) => (
  <StaticQuery
    query={
      graphql`
        query headerTitleQuery {
          site {
            siteMetadata {
              headerTitle
            }
          }
        }
        `}
    render={(data) => {
      const logo = require('./images/react-logo.svg');
      const twitter = require('./images/twitter.svg');
      const {
        site: {
          siteMetadata: {
            headerTitle
          }
        }
      } = data;
      return (
        <div className={'navBarWrapper'}>
          <nav className={'navbar navbar-default navBarDefault'}>
            <div className={'navbar-header'}>
              <button type="button" className={'navbar-toggle collapsed navBarToggle'} data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
                <span className={'sr-only'}>Toggle navigation</span>
                <span className={'icon-bar'}></span>
                <span className={'icon-bar'}></span>
                <span className={'icon-bar'}></span>
              </button>
              <a className={'navbar-brand navBarBrand'} href="/">
                <img className={'img-responsive'} src={logo} alt={'React logo'} />
                {headerTitle}
              </a>
            </div>
            <div id="navbar" className={'navbar-collapse collapse navBarCollapse'}>
              <div className={'visible-xs'}>
                <Sidebar location={location} />
                <hr/>
              </div>
              <ul className={'nav navbar-nav navBarUL'}>
                <li className={'githubBtn'}><a><GitHubButton href="https://github.com/hasura/graphql-engine" data-show-count="true" aria-label="Star hasura/graphql-engine on GitHub">Star</GitHubButton></a>
                </li>
                <li><a href="https://discordapp.com/invite/vBPpJkS">Need Help?</a></li>
              </ul>
              <ul className={'nav navbar-nav navBarUL navbar-right'}>
                <li><a href="https://twitter.com/intent/tweet?&text=Check out this GraphQL course for React developers by @HasuraHQ https://learn.hasura.io/graphql/react" target="_blank"><img className={'twitterIcon'} src={twitter} alt={'Twitter'} /></a></li>
                <li><a href="https://hasura.io/" target="_blank">hasura.io</a></li>
              </ul>
            </div>
          </nav>
        </div>
      );
    }}
  />
);

export default Header;
