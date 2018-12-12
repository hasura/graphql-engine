import React, { Component } from 'react';
import { Query } from "react-apollo";
import gql from "graphql-tag";
import { checkNameExistQuery } from '../queries/Queries';
import $ from 'jquery'
import '../App.css';


class Mark extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    const { name } = this.props;
    return (
      <div>
        <Query query={checkNameExistQuery} variables={{ name }}>
          {
            ({ loading, error, data }) => {
              if(loading)
                return <span>checking ...</span>;
              if(error)
                return <p>error</p>

              if(data.users.length === 0 && name == "")
                return <span></span>;

              if(data.users.length === 0 && name != "")
                return <div>
                          <i className="fas fa-check" style={{ color: 'green', fontSize: '20px' }}></i>
                          <br />
                          <p>username available</p>
                          <br />
                          <p>( press ENTER to SignUp )</p>
                          <br />
                       </div>;

              return <div>
                      <a href={"/dashboard/" + data.users[0].name + "/" + data.users[0].id} className="btn btn-default logged-in">LOGIN</a>
                    </div>;
            }
          }
        </Query>
      </div>
    )
  }
}

export default Mark;
