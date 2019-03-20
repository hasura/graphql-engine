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

class CollapsibleToggleHoc extends React.Component {
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
      <CollapsibleToggle toggle={this.toggle} {...this.props} {...this.state} />
    );
  }
}

const CollapsibleToggle = ({ testId, title, isOpen, toggle, children }) => {
  const styles = require('./CollapsibleToggle.scss');
  return (
    <div className={`${styles.collapsibleWrapper}`}>
      <div
        className={`${styles.collapsibleToggle}`}
        data-test={testId}
        onClick={toggle}
      >
        <span className={`${styles.caretWrapper}`}>
          <i
            className={`fa fa-chevron-right ${
              isOpen ? styles.downArrow : styles.rightArrow
            }`}
            data-test="toggle-column-presets"
          />
        </span>
        <span className={`${styles.titleWrapper}`}>{title}</span>
      </div>
      {isOpen ? (
        <div className={styles.collapsibleContent}>{children}</div>
      ) : null}
    </div>
  );
};

export default CollapsibleToggleHoc;
