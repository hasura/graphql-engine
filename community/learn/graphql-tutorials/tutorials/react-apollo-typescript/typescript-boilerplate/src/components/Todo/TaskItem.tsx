import * as React from 'react'

import { Todo } from './TodoTypeDefs';
interface TaskItemState {
  index: number,
  todo: Todo
}

const TaskItem = ({index, todo} : TaskItemState) => {
  return (
    <li>

      <div className="userInfoPublic" title={todo.user!.name}>
        @{todo.user!.name}
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
