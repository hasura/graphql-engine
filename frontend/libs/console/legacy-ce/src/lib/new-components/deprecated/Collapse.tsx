import React from 'react';
import * as Collapsible from '@radix-ui/react-collapsible';
import { FaChevronRight } from 'react-icons/fa';

import { IconTooltip } from '../Tooltip';

export interface CollapseHeaderProps extends React.HTMLAttributes<HTMLElement> {
  /**
   * The collapse header title
   */
  title?: string;
  /**
   * The collapse header tooltip content
   */
  tooltip?: string;
  /**
   * The collapse header status
   */
  status?: string;
  /**
   * The collapse header disabled status
   */
  disabled?: boolean;
  /**
   * The collapse header disabled status message
   */
  disabledMessage?: string;
}

interface Ctx {
  open: boolean;
}

const CollapseCtx = React.createContext<Ctx>({
  open: false,
});

const CollapseHeader: React.FC<CollapseHeaderProps> = ({
  children,
  title,
  tooltip,
  status,
  disabled = false,
  disabledMessage,
}) => {
  const { open } = React.useContext(CollapseCtx);

  const message = disabled ? disabledMessage : '';

  const buttonClassName = `flex py-1 cursor-pointer gap-2 items-center background-none border-none ${
    disabled ? 'text-gray-500' : ''
  }`;

  const chevronClassName = `transition duration-200 ease-in-out text-gray-600 ${
    open ? 'rotate-90' : 'rotate-0'
  }`;

  if (children) {
    return (
      <Collapsible.Trigger
        type="button"
        disabled={disabled}
        title={message}
        className={buttonClassName}
      >
        <FaChevronRight className={chevronClassName} />
        {children}
      </Collapsible.Trigger>
    );
  }

  return (
    <div className="flex items-center">
      <Collapsible.Trigger
        type="button"
        className={buttonClassName}
        disabled={disabled}
        title={message}
      >
        <FaChevronRight className={chevronClassName} />

        {title && (
          <p className="m-0 text-gray-600">
            <strong>{title}</strong>
          </p>
        )}
      </Collapsible.Trigger>

      <div className="flex items-center gap-2">
        {!!tooltip && <IconTooltip message={tooltip} />}

        {!!status && (
          <span>
            <small>{status}</small>
          </span>
        )}
      </div>
    </div>
  );
};

const CollapseContent: React.FC = ({ children }) => (
  <Collapsible.Content className="my-2 mx-1.5 py-2 px-4 border-solid border-l-2 border-gray-300">
    {children}
  </Collapsible.Content>
);

export interface CollapseProps extends CollapseHeaderProps {
  defaultOpen?: boolean;
  rootClassName?: string;
}

export const Collapse = ({
  children,
  title,
  tooltip,
  status,
  defaultOpen = false,
  disabled = false,
  disabledMessage,
  rootClassName,
  ...props
}: CollapseProps): JSX.Element => {
  const [open, setOpen] = React.useState(defaultOpen);

  const onOpenChange = () => {
    setOpen(prev => !prev);
  };

  return (
    <Collapsible.Root
      open={open}
      onOpenChange={onOpenChange}
      className={rootClassName}
      {...props}
    >
      <CollapseCtx.Provider value={{ open }}>
        {!!title && (
          <CollapseHeader
            title={title}
            tooltip={tooltip}
            disabled={disabled}
            disabledMessage={disabledMessage}
            status={status}
          />
        )}

        {children}
      </CollapseCtx.Provider>
    </Collapsible.Root>
  );
};

Collapse.Header = CollapseHeader;
Collapse.Content = CollapseContent;

export default Collapse;
