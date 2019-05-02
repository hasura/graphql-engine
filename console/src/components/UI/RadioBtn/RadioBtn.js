import React from 'react';
import styles from './RadioBtn.scss';

const RadioBtn = props => {
  const { children } = props;

  return (
    <div>
      <p>
        <input type="radio" id="test1" name="radio-group" checked />
        <label for="test1">Apple</label>
      </p>
      <p>
        <input type="radio" id="test2" name="radio-group" />
        <label for="test2">Peach</label>
      </p>
    </div>
  );
};

export default RadioBtn;
