import clsx from 'clsx';
import React from 'react';
import { BsChevronRight } from 'react-icons/bs';
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

  override componentWillReceiveProps(nextProps: CollapsibleToggleProps) {
    const { isOpen, toggleHandler } = nextProps;

    if (toggleHandler) {
      this.setState({ isOpen: !!isOpen, toggleHandler });
    }
  }

  defaultToggleHandler() {
    this.setState({ isOpen: !this.state.isOpen });
  }

  override render() {
    const { title, children, testId, useDefaultTitleStyle } = this.props;

    const { isOpen, toggleHandler } = this.state;

    const getTitle = () => {
      let resultTitle;

      if (useDefaultTitleStyle) {
        resultTitle = <div className="font-semibold">{title}</div>;
      } else {
        resultTitle = title;
      }

      return resultTitle;
    };

    return (
      <details
        onToggle={(event: React.ChangeEvent<HTMLDetailsElement>) => {
          // it gets called on mount if open=true, so we check if we really need to call handler
          if (event.target.open !== isOpen) {
            toggleHandler();
          }
        }}
        open={isOpen}
      >
        <summary className="cursor-pointer flex items-start" data-test={testId}>
          <span className="inline-block text-xs mr-sm mt-0.5">
            <BsChevronRight
              className={clsx(
                'transition ease-in-out text-gray-600 text-xs stroke-2 ',
                isOpen ? 'rotate-90' : 'rotate-0'
              )}
            />
          </span>
          <span className="inline-block">{getTitle()}</span>
        </summary>
        <div className="mt-sm">{children}</div>
      </details>
    );
  }
}

export default CollapsibleToggle;
