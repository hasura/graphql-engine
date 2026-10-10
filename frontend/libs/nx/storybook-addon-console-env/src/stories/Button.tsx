import './button.css';

type Props = {
  /**
   * Is this the principal call to action on the page?
   */
  primary?: boolean;
  /**
   * What background color to use
   */
  backgroundColor?: string;
  /**
   * How large should the button be?
   */
  size?: 'small' | 'medium' | 'large';
  /**
   * Button contents
   */
  label: string;
  /**
   * Optional click handler
   */
  onClick?: () => void;
};

/**
 * Primary UI component for user interaction
 */
export const Button = ({
  primary = false,
  backgroundColor,
  size = 'medium',
  label,
  ...props
}: Props) => {
  const mode = primary
    ? (window as any).__env?.adminSecretSet
      ? 'storybook-button--primary-2'
      : 'storybook-button--primary'
    : 'storybook-button--secondary';
  return (
    <button
      type="button"
      className={['storybook-button', `storybook-button--${size}`, mode].join(
        ' ',
      )}
      style={backgroundColor ? { backgroundColor } : undefined}
      {...props}
    >
      {label + ' ' + (window as any).__env.consoleType}{' '}
      {(window as any).__env?.adminSecretSet ? (
        <span style={{ fontSize: '18px' }} role="img" aria-label="Lock emoji">
          🔒
        </span>
      ) : null}
      {(window as any).__env?.consoleType.includes('pro') ||
      (window as any).__env.tenantID !== null ? (
        <span style={{ fontSize: '18px' }} role="img" aria-label="Money emoji">
          💰
        </span>
      ) : null}
    </button>
  );
};
