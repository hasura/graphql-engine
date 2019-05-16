import { useEffect, useState } from 'react';
import randomWords from 'random-words';
import gql from 'graphql-tag';

export const getUsername = () => {
  let username = window.localStorage.getItem('graphql:tic-tac-toe:username');
  return username || '';
};

export const getUserid = () => {
  let userid = window.localStorage.getItem('graphql:tic-tac-toe:userid');
  return parseInt(userid, 10);
}

export const useUsername = (client) => {
  const [isReady, setIsReady] = useState(false);
  useEffect(
    () => {
      let username = window.localStorage.getItem('graphql:tic-tac-toe:username');
      if (!username) {
        username = randomWords();
        client.mutate({
          mutation: gql`
            mutation ($name:String) {
              insert_user (
                objects: {
                  name: $name
                }
              ) {
                returning {
                  id
                }
              }
            }
          `,
          variables: {
            name: username
          }
        }).then((resp) => {
          console.log(resp);
          window.localStorage.setItem('graphql:tic-tac-toe:username', username);
          window.localStorage.setItem('graphql:tic-tac-toe:userid', resp.data.insert_user.returning[0].id);
          setIsReady(true);
        })
      } else {
        client.mutate({
          mutation: gql`
            mutation ($name:String, $id: Int!) {
              insert_user (
                objects: {
                  name: $name
                  id: $id
                }
              ) {
                returning {
                  id
                }
              }
            }
          `,
          variables: {
            name: username,
            id: getUserid()
          }
        }).then(r => {
          setIsReady(true);
        }).catch((e) => {
          setIsReady(true);
        })
      }
    },
    [client]
  )
  return isReady;
}

export const joinBoard = (boardId, client) => {
  const JOIN_BOARD = gql`
    mutation ($user_id: Int, $board_id: uuid!) {
      update_board (
        _set: {
          user_2_id: $user_id
        },
        where: {
          _and: {
            id: {
              _eq: $board_id
            },
            user_2_id: {
              _is_null: true
            },
            user_1_id: {
              _neq: $user_id
            }
          }
        }
      ) {
        affected_rows
        returning {
          id
        }
      }
    }
  `;
  return client.mutate({
    mutation: JOIN_BOARD,
    variables: {
      user_id: getUserid(),
      board_id: boardId
    }
  })
}

export const useGameJoiner = (boardId, client) => {
  const [isReady, setIsReady] = useState(false);
  useEffect(
    () => {
      joinBoard(boardId, client).then(() => {
        setIsReady(true);
      }).catch(() => {
        setIsReady(true);
      });
    },
    [boardId, client]
  )
  return isReady;
}


export const useInvitation = () => {

  const [waitingText, setWaitingText] = useState('Invite a friend');
  useEffect(
    () => {
      if (waitingText === 'Invitation copied') {
        setTimeout(
          () => {
            setWaitingText('Invite a friend');
          },
          2000
        );
      }
    },
    [waitingText]
  );

  const copyInviteLink = () => {
    const inviteTextArea = document.createElement('textarea');
    inviteTextArea.value=window.location.href;
    inviteTextArea.setAttribute('readonly', '');
    inviteTextArea.style={position: 'absolute', left: '-9999px'}
    document.body.appendChild(inviteTextArea);
    inviteTextArea.select();
    document.execCommand('copy');
    document.body.removeChild(inviteTextArea)
    setWaitingText('Invitation copied');
  }

  return {waitingText, copyInviteLink};
}














