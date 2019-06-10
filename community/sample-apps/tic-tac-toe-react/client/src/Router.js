import React from 'react';
import Boards from './components/Boards/BoardsWrapper';
import Play from './components/Game/Play';

const Router = () => {
  const curPath = window.location.pathname
  const searchParams = new URLSearchParams(window.location.search);

  if (curPath === '/play' && searchParams.get('board_id')) {
    return (
      <div className="margin-twenty">
      <Play boardId={searchParams.get('board_id')} />
      </div>
    )
  }

  if (curPath !== '/') {
    window.location.replace(window.location.origin);
    return null;
  }

  return (
    <div className="margin-twenty">
      <Boards />
    </div>
  )

};

export default Router;
