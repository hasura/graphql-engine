import React from 'react';
import { getUserid } from '../../utils';

const postitionCoordinates = {
  '0': '(1, 1)',
  '1': '(1, 2)',
  '2': '(1, 3)',
  '3': '(2, 1)',
  '4': '(2, 2)',
  '5': '(2, 3)',
  '6': '(3, 1)',
  '7': '(3, 2)',
  '8': '(3, 3)',
};

const Logs = ({board}) => {

  if (!board.moves.length) {
    return null;
  }

  const userId = getUserid();

  const moveLogs = board.moves.map(m => {
    const move = m.user_id === board.user1.id ? 'x' : 'o';
    return (
      <div><b>{m.user.id === userId ? 'You' : m.user.name}</b> played <b>{move}</b> at position <b>{postitionCoordinates[m.position]}</b></div>
    )
  });

  return (
    <div className="logs black-border flex-1 width-400">
    {
      moveLogs.map(l => {
        return (
          <div>
            <div key={l} className="margin-bottom-mid">{l}</div>
          </div>
        );
      })
    }
    </div>
  )
};

export default Logs;