import React from 'react';
import { Subscription, withApollo } from 'react-apollo'
import { getUserid, useGameJoiner, useInvitation } from '../../utils';
import gql from 'graphql-tag';
import Logs from './Logs';
import TicTacToe from './TicTacToe';
import { LoadingIndicator } from '../Utils';

const SUBSCRIBE_TO_BOARD = gql`
  subscription($board_id: uuid!) {
    board: board_by_pk (id: $board_id) {
      id
      moves (order_by: { id: desc}) {
        id
        position
        user {
          id
          name
        }
        user_id
      }
      user1 {
        id
        name
      }
      user2 {
        id
        name
      }
      turn
      winner
    }
  }
`

const Play = ({boardId, client}) => {
  const userId = getUserid();
  const isReady = useGameJoiner(boardId, client);
  const {waitingText, copyInviteLink} = useInvitation();
  if (!isReady) {
    return "Please wait";
  }

  return (
    <Subscription
      subscription={SUBSCRIBE_TO_BOARD}
      variables={{ board_id: boardId}}
    >
      {
        ({data, error, loading}) => {
          if (error) {
            console.error(error);
            return "Error";
          }

          if (loading) {
            return <LoadingIndicator />
          }

          const { board } = data;

          const isSelfUser1 = board.user1.id === userId;

          if (!board.user2 && !isSelfUser1) {
            return "Unauthorized user";
          }
          if (board.user2 && board.user2.id !== userId && !isSelfUser1) {
            return "Unauthorized user";
          }

          const _self = (
            <div>
              <h4>
                You play <b>{isSelfUser1 ? 'X' : 'O'}</b>
              </h4>
            </div>
          );

          const opponent = (
            board.user2 ?
            <h4> {isSelfUser1 ? board.user2.name : board.user1.name} plays <b>{isSelfUser1 ? 'O' : 'X'}</b> </h4> :
            <h4> <i> <a className="cursor-pointer color-blue" onClick={copyInviteLink}>{waitingText}</a> </i></h4>
          )

          let status;
          if (board.winner) {
            const isSelfWinner = (board.winner === 'x' && isSelfUser1) || (board.winner === 'o' && !isSelfUser1);
            const winner = `${isSelfWinner ? "You win!!" : `${isSelfUser1 ? board.user2.name : board.user1.name} wins :(`}`
            status= <h2 style={{color: isSelfWinner ? 'green' : 'red'}}>{winner}</h2>
          } else {
            if (board.turn === 'x') {
              status = <h2 style={{color: isSelfUser1 ? 'green' : '#565656'}}>{isSelfUser1 ? 'Your' : `${board.user2.name}'s`} turn</h2>
            } else {
              status = <h2 style={{color: !isSelfUser1 ? 'green' : '#565656'}}>{!isSelfUser1 ? 'Your' : `${board.user1.name}'s`} turn</h2>
            }
          }

          const statusWrapper = board.user2 ? status : <h2> Waiting for an opponent </h2>

          return (
            <div className="display-flex-spread">
              <div className="display-flex-center">
                <div className="width-300 text-align-center">{_self}</div>
              </div>
              <div className="flex-1">
                <div className="display-flex-center margin-bottom">
                  {statusWrapper}
                </div>
                <TicTacToe board={board} client={client}/>
                <Logs board={board}/>
              </div>
              <div className="display-flex-center">
                <div className="width-300 text-align-center">{opponent}</div>
              </div>
            </div>
          )
        }
      }
    </Subscription>
  )
};

export default withApollo(Play);
