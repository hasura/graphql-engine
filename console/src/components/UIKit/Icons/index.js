import React from 'react';
import PropTypes from 'prop-types';

// Success Icon
import { FaCheckCircle } from 'react-icons/fa';

// Information Icon
import { FaInfoCircle } from 'react-icons/fa';

// Warning Icon
import { FaExclamationTriangle } from 'react-icons/fa';

// Error Icon
import { FaExclamationCircle } from 'react-icons/fa';

// ******************************************* //

const Icon = ({ iconType }) => {
  const iconsReferenceObject = {
    success: FaCheckCircle,
    info: FaInfoCircle,
    warning: FaExclamationTriangle,
    error: FaExclamationCircle,
    default: FaExclamationCircle,
  };

  // If the iconType is out of range then default icon will be assign to ActiveIcon.

  const ActiveIcon = iconsReferenceObject[iconType]
    ? iconsReferenceObject[iconType]
    : iconsReferenceObject.default;

  return <ActiveIcon />;
};

// PropTypes for Icons *************** //

Icon.propTypes = {
  iconType: PropTypes.string.isRequired,
};

export default Icon;
