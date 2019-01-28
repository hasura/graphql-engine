import React from 'react';
import { Subscription } from 'react-apollo';
import moment from 'moment';
import gql from 'graphql-tag';

const fetchOnlineUsersSubscription = gql`
  subscription {
    user_online (
      order_by: {username:asc}
    ) {
      id
      username
    }
  }
`;

class OnlineUsers extends React.Component {

  constructor(props) {
    super(props);
    this.state = {
      time: moment().subtract(10, 'seconds').format(),
      refetch: null,
      showMobileView: false
    }
  }

  toggleMobileView = () => {
    this.setState({
      ...this.state,
      showMobileView: !this.state.showMobileView
    });
  }

  render() {

    const subscriptionData = (isMobileView) => (
      <Subscription
        subscription={fetchOnlineUsersSubscription}
      >
        {
          ({data, error, loading }) => {
            if (loading) {
              return null;
            }
            if (error) { return "Error loading online users"; }
            return (
              <div>
                <p
                  className={ isMobileView ? "mobileuserListHeading" : "userListHeading"}
                  onClick={this.toggleMobileView}
                >
                  Online Users ({!data.user_online ? 0 : data.user_online.length}) { isMobileView && (<i className="fa fa-angle-up"></i>)}
                </p>
                {
                  ((isMobileView && this.state.showMobileView) || !isMobileView) &&
                  (
                    <ul className={isMobileView ? "mobileUserList" : "userList"}>
                      {
                        data.user_online.map((u) => {
                          return <li key={u.id}>{u.username}</li>
                        })
                      }
                    </ul>
                  )
                }
                
              </div>
            );
          }
        }
      </Subscription>
    );

    return (
      <div>
        <div className="onlineUsers hidden-xs">
          {subscriptionData(false)}
        </div>
        <div className="mobileonlineUsers visible-xs">
          {subscriptionData(true)}
        </div>
      </div>
    );
  }
};

export default OnlineUsers;
