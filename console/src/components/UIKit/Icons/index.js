import React from 'react';
import PropTypes from 'prop-types';

// Icons ************************************ //

// Success Icon
import { FaCheckCircle } from 'react-icons/fa';

// Information Icon
import { FaInfoCircle } from 'react-icons/fa';

// Warning Icon
import { FaExclamationTriangle } from 'react-icons/fa';

// Error Icon
import { FaExclamationCircle } from 'react-icons/fa';

// ******************************************* //

import { IconStyles } from './Icon.style';

// ******************************************* //

const DynamicIcon = ({ type }) => {
  // All icons references

  const iconsReferenceObject = {
    success: FaCheckCircle,
    info: FaInfoCircle,
    warning: FaExclamationTriangle,
    error: FaExclamationCircle,
    default: FaExclamationCircle,
  };

  // If the icon type is out of range then default icon will be assign to ActiveIcon.

  const ActiveIcon = iconsReferenceObject[type]
    ? iconsReferenceObject[type]
    : iconsReferenceObject.default;

  return <ActiveIcon />;
};

// PropTypes for DynamicIcon *************** //

DynamicIcon.propTypes = {
  type: PropTypes.string.isRequired,
};

// ******************************************* //

const Icon = props => {
  const { type } = props;

  return (
    <IconStyles {...props}>
      <DynamicIcon type={type} />
    </IconStyles>
  );
};

// PropTypes for Icons *************** //

Icon.propTypes = {
  type: PropTypes.string.isRequired,
  fontSize: PropTypes.string,
};

// Default props for Icon ******************* //

Icon.defaultProps = {
  fontSize: 'icon',
  width: 18,
  height: 18,
};

// ****************************************** //

export { Icon };
