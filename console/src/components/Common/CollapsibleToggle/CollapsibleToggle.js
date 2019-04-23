import React from 'react';

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
    const styles = require('./CollapsibleToggle.scss');

    const { title, children, testId, useDefaultTitleStyle } = this.props;

    const { isOpen, toggleHandler } = this.state;

    const getTitle = () => {
      let _title;

      if (useDefaultTitleStyle) {
        _title = <div className={styles.collapsibleTitle}>{title}</div>;
      } else {
        _title = title;
      }

      return _title;
    };

    const getChildren = () => {
      let _children;

      if (isOpen) {
        _children = <div className={styles.collapsibleContent}>{children}</div>;
      }

      return _children;
    };

    const getIndicatorType = () => {
      let _indicatorStateStyle;

      if (isOpen) {
        _indicatorStateStyle = styles.collapsibleIndicatorOpen;
      }

      return _indicatorStateStyle;
    };

    return (
      <div className={styles.collapsibleWrapper}>
        <div
          className={styles.collapsibleToggle}
          data-test={testId}
          onClick={toggleHandler}
        >
          <span className={styles.collapsibleIndicatorWrapper}>
            <i
              className={`fa fa-chevron-right ${
                styles.collapsibleIndicator
              } ${getIndicatorType()}`}
            />
          </span>

          <span className={styles.titleWrapper}>{getTitle()}</span>
        </div>

        {getChildren()}
      </div>
    );
  }
}

export default CollapsibleToggle;
