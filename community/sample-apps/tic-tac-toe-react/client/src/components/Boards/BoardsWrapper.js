import React from 'react';
import gql from 'graphql-tag';
import { Subscription, withApollo } from 'react-apollo';
import BoardsList from './BoardsList'
import { LoadingIndicator } from '../Utils';

const SUBSCRIBE_TO_BOARDS = gql`
  subscription {
    board (
      where: {
        _and: {
          winner: {
            _is_null: true
          },
          user_2_id: {
            _is_null: true
          }
        }
      }
      order_by: {
        created_at: asc
      }
    ) {
      id
      user1 {
        id
        name
      }
      user_2_id
      created_at
      winner
    }
  }
`;

const Boards = ({client}) => {
  return (
    <Subscription
      subscription={SUBSCRIBE_TO_BOARDS}
    >
      {
        ({data, error, loading}) => {
          if (error) {
            console.error(error);
            return "Error";
          }

          if (loading) return <LoadingIndicator />;

          return <BoardsList boards={data.board}/>
        }
      }
    </Subscription>
  );
}

export default withApollo(Boards);
