import React, { Component } from 'react';
import { Query } from "react-apollo";
import { fetchUserQuery } from '../queries/Queries';

class Navbar extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    const { name } = this.props;
    return (
      <nav className="navbar navbar-default">
        <div className="container-fluid">
          <div className="navbar-header">
            <button type="button" className="navbar-toggle collapsed" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1" aria-expanded="false">
              <span className="sr-only">Toggle navigation</span>
              <span className="icon-bar"></span>
              <span className="icon-bar"></span>
              <span className="icon-bar"></span>
            </button>
            <a className="navbar-brand" href="#">basic</a>
          </div>

          <div className="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
            <ul className="nav navbar-nav navbar-right">
            <a href="#" className="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">
            <Query query={fetchUserQuery} variables={{ name }}>
              {
                ({ loading, error, data }) => {
                  if(loading)
                    return <span>loading ...</span>;
                  if(error)
                    return <p>error</p>

                  return <span>{data.users[0].name}</span>;
                }
              }
            </Query>
            <span className="caret"></span></a>
              <ul className="dropdown-menu">
                <li><a href="/">Logout</a></li>
              </ul>
            </ul>
          </div>
        </div>
      </nav>
    )
  }
}


export default Navbar;
