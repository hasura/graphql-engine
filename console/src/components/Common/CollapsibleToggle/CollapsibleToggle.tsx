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
  testId?: string;
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

    return (
      <details
        className={styles.collapsibleWrapper}
        onToggle={(event: React.ChangeEvent<HTMLDetailsElement>) => {
          // it gets called on mount if open=true, so we check if we really need to call handler
          if (event.target.open !== isOpen) {
            toggleHandler();
          }
        }}
        open={isOpen}
      >
        <summary className={styles.collapsibleToggle} data-test={testId}>
          <span className={styles.collapsibleIndicatorWrapper}>
            <i
              className={`fa fa-chevron-right ${styles.collapsibleIndicator} ${
                isOpen && styles.collapsibleIndicatorOpen
              }`}
            />
          </span>

          <span className={styles.titleWrapper}>{getTitle()}</span>
        </summary>
        <div className={styles.collapsibleContent}>{children}</div>
      </details>
    );
  }
}

export default CollapsibleToggle;
