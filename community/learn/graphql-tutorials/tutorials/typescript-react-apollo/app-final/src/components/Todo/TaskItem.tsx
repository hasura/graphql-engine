import * as React from 'react'

import { Todos } from '../../generated/graphql';

type User = {
  user: {
    name: string
  }
}

interface TaskItemState {
  index: number,
  todo: Partial<Todos> & User
}

const TaskItem = ({index, todo} : TaskItemState) => {
  return (
    <li>
      <div className="userInfoPublic" title={todo.user.name}>
        @{todo.user.name}
      </div>
      <div className={"labelContent" + (todo.is_completed ? " completed" : '')}>
        <div>
          {todo.title}
        </div>
      </div>
    </li>
  );
};

export default TaskItem;
