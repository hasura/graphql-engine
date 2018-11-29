import React from 'react';

const Buttons = ({ styles }) => (
  <div className={styles.add_mar_bottom}>
    <button className={'btn btn-sm btn-danger'} data-test="delete-trigger">
      Delete Trigger
    </button>
  </div>
);

export default Buttons;
