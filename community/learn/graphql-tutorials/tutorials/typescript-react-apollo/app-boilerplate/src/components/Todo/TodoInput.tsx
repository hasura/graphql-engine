import * as React from 'react';

const TodoInput = ({isPublic=false}) => {
  const [todoInput, setTodoInput] = React.useState('');
  return (
    <form className="formInput" onSubmit={(e) => {
      e.preventDefault();
      // add todo
    }}>
      <input
        className="input"
        placeholder="What needs to be done?"
        value={todoInput}
        onChange={e => (setTodoInput(e.target.value))}
      />
      <i className="inputMarker fa fa-angle-right" />
    </form>
  );
};

export default TodoInput;
