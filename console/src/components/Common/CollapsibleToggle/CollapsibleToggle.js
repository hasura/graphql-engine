import React from 'react';

import { Icon } from '../../UIKit/atoms';
import styles from './CollapsibleToggle.scss';

/**
 *  Accepts following props
 *    `title, string || react-element `: Title of the collapsible toggle
 *    `isOpen`(optional, default to false): Whether the body should be shown or not
 *    `toggleHandler (optional)`: Function to call when the toggle is clicked
 *    `testId, string`: Test identifier
 *    `children, react-element`: The content which needs to be toggled
 */

class CollapsibleToggle extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      isOpen: props.isOpen || false,
      toggleHandler:
        props.toggleHandler || this.defaultToggleHandler.bind(this),
    };
  }

  defaultToggleHandler() {
    this.setState({ isOpen: !this.state.isOpen });
  }

  componentWillReceiveProps(nextProps) {
    const { isOpen, toggleHandler } = nextProps;

    if (toggleHandler) {
      this.setState({ isOpen: isOpen, toggleHandler: toggleHandler });
    }
  }

  render() {
    const { title, children, testId, useDefaultTitleStyle } = this.props;

    const { isOpen, toggleHandler } = this.state;

    const getTitle = () => {
      let _title;

      if (useDefaultTitleStyle) {
        _title = <div className={styles.defaultCollapsibleTitle}>{title}</div>;
      } else {
        _title = title;
      }

      return _title;
    };

    const getChildren = () => {
      return <div className={styles.collapsibleContent}>{children}</div>;
    };

    return (
      <div className={styles.collapsibleWrapper}>
        <div
          className={styles.collapsibleToggle}
          data-test={testId}
          onClick={toggleHandler}
        >
          <Icon
            type="right"
            size={12}
            mr="sm"
            className={`${styles.collapsibleIndicator} ${isOpen &&
              styles.collapsibleIndicatorOpen}`}
          />
          <span className={styles.titleWrapper}>{getTitle()}</span>
        </div>

        {isOpen && getChildren()}
      </div>
    );
  }
}

export default CollapsibleToggle;
