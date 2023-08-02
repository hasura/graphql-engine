import React from 'react';
import PropTypes from 'prop-types';
import './button.css';

/**
 * Primary UI component for user interaction
 */
export const Button = ({ primary, backgroundColor, size, label, ...props }) => {
  const mode = primary
    ? window.__env?.adminSecretSet
      ? 'storybook-button--primary-2'
      : 'storybook-button--primary'
    : 'storybook-button--secondary';
  return (
    <button
      type="button"
      className={['storybook-button', `storybook-button--${size}`, mode].join(
        ' '
      )}
      style={backgroundColor && { backgroundColor }}
      {...props}
    >
      {label + ' ' + window.__env.consoleType}{' '}
      {window.__env?.adminSecretSet ? (
        <span style={{ fontSize: '18px' }} role="img" aria-label="Lock emoji">
          🔒
        </span>
      ) : null}
      {window.__env?.consoleType.includes('pro') ||
      window.__env.tenantID !== null ? (
        <span style={{ fontSize: '18px' }} role="img" aria-label="Money emoji">
          💰
        </span>
      ) : null}
    </button>
  );
};

Button.propTypes = {
  /**
   * Is this the principal call to action on the page?
   */
  primary: PropTypes.bool,
  /**
   * What background color to use
   */
  backgroundColor: PropTypes.string,
  /**
   * How large should the button be?
   */
  size: PropTypes.oneOf(['small', 'medium', 'large']),
  /**
   * Button contents
   */
  label: PropTypes.string.isRequired,
  /**
   * Optional click handler
   */
  onClick: PropTypes.func,
};

Button.defaultProps = {
  backgroundColor: null,
  primary: false,
  size: 'medium',
  onClick: undefined,
};
