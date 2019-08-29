import React from "react";

const TaskItem = ({ index, todo }) => {
  return (
    <li>
      <div className="userInfoPublic" title={todo.user.name}>
        @{todo.user.name}
      </div>

      <div className={"labelContent" + (todo.is_completed ? " completed" : "")}>
        <div>{todo.title}</div>
      </div>
    </li>
  );
};

export default TaskItem;
