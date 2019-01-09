import React from 'react';
import './styles.css';
const headerTitle = process.env.GATSBY_HEADER_TITLE;
import Sidebar from "./sidebar";
class Header extends React.Component {
  render() {
    const logo = require('./images/react-logo.svg');
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
              <Sidebar location={this.props.location} />
              <hr/>
            </div>
            <ul className={'nav navbar-nav navBarUl'}>
              <li><a href="https://github.com/hasura/graphql-engine">GitHub</a></li>
              <li><a href="https://discordapp.com/invite/vBPpJkS">Discord</a></li>
              <li><a href="#">Need Help?</a></li>
            </ul>
          </div>
        </nav>
      </div>
    );
  }
}

export default Header;
