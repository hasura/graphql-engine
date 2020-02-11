import React from 'react';
import PropTypes from 'prop-types';

// Icons ************************************ //

import {
  FaCheckCircle,
  FaFlask,
  FaInfoCircle,
  FaExclamationTriangle,
  FaExclamationCircle,
  FaDatabase,
  FaPlug,
  FaCloud,
  FaCog,
  FaQuestion,
} from 'react-icons/fa';

// Theme Object ****************************** //

import { theme } from '../theme';

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
    graphiql: FaFlask,
    database: FaDatabase,
    schema: FaPlug,
    event: FaCloud,
    settings: FaCog,
    question: FaQuestion,
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
  const { type, color } = props;

  // We can override the icon default color by passing a color prop.

  let iconColor;

  // If color prop is true then it's assigned to iconColor variable else default theme color value is assigned.

  if (color) {
    iconColor = color;
  } else {
    iconColor = theme.icons[type]
      ? theme.icons[type].color
      : theme.icons.default.color;
  }

  // ************************ //

  return (
    <IconStyles {...props} color={iconColor}>
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
