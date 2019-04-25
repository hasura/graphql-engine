import PropTypes from 'prop-types';
import React, { Component } from 'react';
import { connect } from 'react-redux';

import Button from '../Button/Button';
import RadioBtn from '../RadioBtn/RadioBtn';

class UITest extends Component {
  render() {
    const styles = require('./UITest.scss');

    return (
      <div className={styles.p20}>
        <h1 className={styles.mb20}>Heading 1</h1>
        <h2 className={styles.mb20}>Heading 2</h2>
        <p className={styles.mb20}>paragraph</p>
        <div className={styles.mb20}>
          <Button type="primary">Primary Btn</Button>
        </div>
        <div>
          <RadioBtn>Radio Btn</RadioBtn>
        </div>
      </div>
    );
  }
}

UITest.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

export default connect()(UITest);
