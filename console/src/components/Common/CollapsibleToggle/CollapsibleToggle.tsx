import React from 'react';
import styles from './CollapsibleToggle.scss';
/**
 *  Accepts following props
 *    `title, string || react-element `: Title of the collapsible toggle
 *    `isOpen`(optional, default to false): Whether the body should be shown or not
 *    `toggleHandler (optional)`: Function to call when the toggle is clicked
 *    `testId, string`: Test identifier
 *    `children, react-element`: The content which needs to be toggled
 */

interface CollapsibleToggleProps {
  title: React.ReactNode;
  isOpen?: boolean;
  toggleHandler?: () => void;
  testId: string;
  useDefaultTitleStyle?: boolean;
}

interface CollapsibleToggleState {
  isOpen: boolean;
  toggleHandler: () => void;
}

class CollapsibleToggle extends React.Component<
  CollapsibleToggleProps,
  CollapsibleToggleState
> {
  constructor(props: CollapsibleToggleProps) {
    super(props);

    this.state = {
      isOpen: props.isOpen || false,
      toggleHandler:
        props.toggleHandler || this.defaultToggleHandler.bind(this),
    };
  }

  componentWillReceiveProps(nextProps: CollapsibleToggleProps) {
    const { isOpen, toggleHandler } = nextProps;

    if (toggleHandler) {
      this.setState({ isOpen: !!isOpen, toggleHandler });
    }
  }

  defaultToggleHandler() {
    this.setState({ isOpen: !this.state.isOpen });
  }

  render() {
    const { title, children, testId, useDefaultTitleStyle } = this.props;

    const { isOpen, toggleHandler } = this.state;

    const getTitle = () => {
      let resultTitle;

      if (useDefaultTitleStyle) {
        resultTitle = (
          <div className={styles.defaultCollapsibleTitle}>{title}</div>
        );
      } else {
        resultTitle = title;
      }

      return resultTitle;
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
          onKeyDown={toggleHandler}
          role="button"
          tabIndex={0}
        >
          <span className={styles.collapsibleIndicatorWrapper}>
            <i
              className={`fa fa-chevron-right ${styles.collapsibleIndicator} ${
                isOpen && styles.collapsibleIndicatorOpen
              }`}
            />
          </span>

          <span className={styles.titleWrapper}>{getTitle()}</span>
        </div>

        {isOpen && getChildren()}
      </div>
    );
  }
}

export default CollapsibleToggle;
