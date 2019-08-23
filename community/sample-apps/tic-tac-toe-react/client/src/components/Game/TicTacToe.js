import React from 'react';
import { Mutation, withApollo } from 'react-apollo';
import { getUserid } from '../../utils';
import gql from 'graphql-tag';

const MAKE_MOVE = gql`
  mutation (
    $board_id: String!,
    $position: Int!,
    $user_id: Int!
  ) {
    make_move (
      board_id: $board_id,
      position: $position,
      user_id: $user_id
    ) {
      success
    }
  }
`;

const TicTacToe = ({board, client}) => {
  const {
    moves,
    user1,
    user2,
    turn,
    winner
  } = board;
  const positions = Array(9).fill();
  moves.forEach((m) => {
    positions[m.position] = m.user_id === user1.id ? 'x' : 'o';
  });
  const selfUserId = getUserid();
  return (
    <div className="display-flex-center">
      <table className="game-board active">
        <tbody>
        {
          [0, 3, 6].map((i) => {
            return (
              <tr key={`tr-${i}`}>
                {
                  [0, 1, 2].map((j) => {
                    return (
                      <Mutation
                        mutation={MAKE_MOVE}
                        variables={{
                          board_id: board.id,
                          user_id: selfUserId,
                          position: i + j
                        }}
                        key={`td-${i+j}`}
                      >
                        {
                          (mutate, { data, error, loading }) => {
                            const isSelfTurn = () => {
                              if (!board.user2) {
                                return false;
                              }
                              if (
                                loading ||
                                (turn === 'o' && user1.id === selfUserId) ||
                                (turn === 'x' && user2.id === selfUserId) ||
                                Boolean(winner)
                              ) {
                                return false;
                              }
                              return true;
                            }
                            let cursorType = isSelfTurn() ? "cursor-pointer" : "cursor-forbidden";
                            const makeMove = () => {
                              if (!isSelfTurn()) {
                                return;
                              }
                              mutate();
                            }


                            return (
                              <td onClick={makeMove} disabled={loading} className={`grid-cell ${cursorType}`}>
                                {positions[i+j] || ' '}
                              </td>
                            )
                          }
                        }
                      </Mutation>
                    )
                  })
                }
              </tr>
            )
          })
        }
        </tbody>
      </table>
    </div>
  );
}

export default withApollo(TicTacToe);
