import React, { Fragment, useState, useRef } from "react";
import TaskItem from "./TaskItem";

type TodoItem = {
  id: number,
  title: string,
  user: { name: string }
}

type publicListProps = {
  latestTodo?: TodoItem | null
}

const TodoPublicList = (props: publicListProps) => {
  const [olderTodosAvailable] = useState(props.latestTodo ? true : false)
  const [newTodosCount] = useState(0)
  const initialTodos = [
    {
      id: 1,
      title: "This is public todo 1",
      user: {
        name: "someUser1"
      }
    },
    {
      id: 2,
      title: "This is public todo 2",
      is_completed: false,
      is_public: true,
      user: {
        name: "someUser2"
      }
    },
    {
      id: 3,
      title: "This is public todo 3",
      user: {
        name: "someUser3"
      }
    },
    {
      id: 4,
      title: "This is public todo 4",
      user: {
        name: "someUser4"
      }
    }
  ];
  const [todos] = useState<TodoItem[]>(initialTodos);

  let oldestTodoId = useRef(props.latestTodo ? props.latestTodo.id + 1 : 0);
  let newestTodoId = useRef(props.latestTodo ? props.latestTodo.id : 0);
  if(todos && todos.length) {
    oldestTodoId.current = todos[todos.length - 1].id
    newestTodoId.current = todos[0].id;
  }

  const loadOlder = () => {
  };

  const loadNew = () => {
  };

  return (
    <Fragment>
      <div className="todoListWrapper">
        {newTodosCount !== 0 && (
          <div className={"loadMoreSection"} onClick={() => loadNew()}>
            New tasks have arrived! ({newTodosCount.toString()})
          </div>
        )}

        <ul>
          {todos &&
            todos.map((todo, index) => {
              return <TaskItem key={index} index={index} todo={todo} />;
            })}
        </ul>

        <div className={"loadMoreSection"} onClick={() => loadOlder()}>
          {olderTodosAvailable
            ? "Load older tasks"
            : "No more public tasks!"}
        </div>
      </div>
    </Fragment>
  );
};

export default TodoPublicList;