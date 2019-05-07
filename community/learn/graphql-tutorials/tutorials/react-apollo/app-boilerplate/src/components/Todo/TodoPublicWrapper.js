import React from 'react';

import TodoInput from './TodoInput';
import TodoPublicList from './TodoPublicList';

const TodoPublicWrapper = () => {
  return (
    <div className="todoWrapper">
      <div className="sectionHeader">Public feed (realtime)</div>

      <TodoInput isPublic />
      <TodoPublicList />
    </div>
  );
};

export default TodoPublicWrapper;
