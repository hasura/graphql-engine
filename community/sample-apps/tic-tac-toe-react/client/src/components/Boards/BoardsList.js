import React from 'react';
import { ListGroup } from 'react-bootstrap';
import gql from 'graphql-tag';
import { withApollo } from 'react-apollo';
import { getUserid, getUsername } from '../../utils';

const CREATE_BOARD = gql`
  mutation ($user_id: Int) {
    insert_board (
      objects: [{
        user_1_id: $user_id,
        turn: "x",
      }]
    ) {
      returning {
        id
      }
    }
  }
`;

const BoardsList = ({ boards, client }) => {
  const createBoard = () => {
    client.mutate({
      mutation: CREATE_BOARD,
      variables: {
        user_id: getUserid(),
      }
    }).then((response) => {
      const { data: { insert_board } } = response;
      const boardId = insert_board.returning[0].id;
      window.location.replace(`${window.location.origin}/play?board_id=${boardId}`)
    }).catch(e => {
      console.error(e);
      alert('Unexpected error! Please try again.')
    })
  }
  if (boards.length === 0) {
    return (
      <div className="display-flex-center">
        <h3>No active boards. <a className="cursor-pointer color-blue" onClick={createBoard}>Create one</a>.</h3>
      </div>
    );
  }
  return (
    <div>
      <h3 className="margin-bottom">Join a board or <a className="cursor-pointer color-blue" onClick={createBoard}>create one</a></h3>
      <ListGroup>
        {
          boards.map(({created_at, id, user1, user_1_move}) => {
            const joinBoard = () => {
              window.location.replace(`${window.location.origin}/play?board_id=${id}`);
            }
            return (
              <ListGroup.Item className="cursor-pointer" onClick={() => joinBoard(id)}>
                <i class="fas fa-th" />&nbsp;&nbsp;&nbsp;&nbsp;Created by <b>{user1.id === getUserid() ? "you" : user1.name}</b>
              </ListGroup.Item>
            )
          })
        }
      </ListGroup>
    </div>
  )
}

export default withApollo(BoardsList);
