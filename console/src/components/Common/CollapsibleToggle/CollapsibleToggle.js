import React from 'react';

/**
 * CollapsibleToggleHoc should do the following
 *  i)   It should provide an interface using which it is easy to add a collapsible widget wherever it is required
 *  ii) Accepts following props
 *    `testId, string`: Test identifier
 *    `title, string || react-element `: Title of the collapsible toggle
 *    `isOpen`(optional, default to false): Whether the body should be shown or not
 *    `toggle`: Function to call when the toggle is clicked
 *    `children, react-element`: The content which needs to be toggled
 */

class CollapsibleToggle extends React.Component {
  constructor(props) {
    super(props);

    this.state = {};
    this.state.isOpen = props.isOpen || false;

    this.toggle = this.toggle.bind(this);
  }

  toggle() {
    this.setState({ isOpen: !this.state.isOpen });
  }

  render() {
    return (
      <CollapsibleToggleComponent
        toggle={this.toggle}
        {...this.props}
        {...this.state}
      />
    );
  }
}

const CollapsibleToggleComponent = ({
  title,
  children,
  isOpen,
  toggle,
  testId,
  defaultTitle,
}) => {
  const styles = require('./CollapsibleToggle.scss');

  const getTitle = () => {
    let _title;

    if (defaultTitle) {
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
        onClick={toggle}
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
};

export default CollapsibleToggle;
