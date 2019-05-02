import PropTypes from 'prop-types';
import React, { Component } from 'react';
import { connect } from 'react-redux';

import Button from '../Button/Button';
import RadioBtn from '../RadioBtn/RadioBtn';
import SwitchButton from '../SwitchButton/SwitchButton';
import Alert from '../Alert/Alert';
import Navigation from '../Navigation/Navigation';
class UITest extends Component {
  render() {
    const styles = require('./UITest.scss');

    return (
      <div className={styles.p20}>
        <h1 className={styles.mb20 + ' ' + styles.mainHeader}>Heading 1</h1>
        <h2 className={styles.mb20 + ' ' + styles.pageHeader}>Heading 2</h2>
        <h3 className={styles.mb20 + ' ' + styles.sectionHeader}>Heading 3</h3>
        <h4 className={styles.mb20 + ' ' + styles.subSectionHeader}>Heading 4</h4>
        <h5 className={styles.mb20 + ' ' + styles.bodyContentLabel}>Heading 5</h5>
        <p className={styles.mb20 + ' ' + styles.bodyContent}>paragraph</p>
        <p className={styles.mb20 + ' ' + styles.explainerContent}>explainer</p>
        <div className={styles.mb20}>
          <Button size="large" type="success">Primary Btn</Button>
        </div>
        <div className={styles.mb20}>
          <Button size="small" type="secondary">Socondary Btn</Button>
        </div>
        <div className={styles.mb20}>
          <RadioBtn>Radio Btn</RadioBtn>
        </div>
        <div className={styles.mb20}>
          <SwitchButton />
        </div>
        <div className={styles.mb20}>
          <div className={styles.widHeight + ' ' + styles.boxShadow}>
          </div>
        </div>
        <div className={styles.mb20}>
          <Alert type="alertSuccess">You did something awesome. Well done!</Alert>
        </div>
        <div className={styles.mb20}>
          <Alert type="alertWarning">You did something wrong.</Alert>
        </div>
        <div className={styles.mb20}>
          <Navigation
          />
        </div>
      </div>
    );
  }
}

UITest.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

export default connect()(UITest);
