import PropTypes from 'prop-types';
import React, { Component } from 'react';
import { connect } from 'react-redux';

import Button from '../Button/Button';
import RadioBtn from '../RadioBtn/RadioBtn';
import CheckBox from '../CheckBox/CheckBox';
import SwitchButton from '../SwitchButton/SwitchButton';
import Alert from '../Alert/Alert';
import ReusableTabs from '../ReusableTabs/ReusableTabs';
import SelectOption from '../SelectOption/SelectOption';
class UITest extends Component {
  render() {
    const styles = require('./UITest.scss');
    const tabs = [
      {
        title: 'Title 1',
        tabContent: 'Content 1',
      },
      {
        title: 'Title 2',
        tabContent: 'Content 2',
      },
      {
        title: 'Title 3',
        tabContent: 'Content 3',
      }
    ]
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
          <RadioBtn>Radio1</RadioBtn>
        </div>
        <div className={styles.mb20}>
          <RadioBtn>Radio2</RadioBtn>
        </div>
        <div className={styles.mb20}>
          <CheckBox>checkbox1</CheckBox>
        </div>
        <div className={styles.mb20}>
          <CheckBox>checkbox2</CheckBox>
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
          <ReusableTabs tabs={tabs}
          />
        </div>
        <div className={styles.mb20}>
          <SelectOption />
        </div>
      </div>
    );
  }
}

UITest.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

export default connect()(UITest);
