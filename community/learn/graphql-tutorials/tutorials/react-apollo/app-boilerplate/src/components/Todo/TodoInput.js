import React from 'react';

const TodoInput = ({type}) => {
  return (
    <form className="formInput" onSubmit={(e) => {
      e.preventDefault();
    }}>
      <input
        className="input"
        data-test={type === "private" ? "input-private" : "input-public"}
        placeholder="What needs to be done?"
      />
      <i className="inputMarker fa fa-angle-right" />
    </form>
  );
}

export default TodoInput;
