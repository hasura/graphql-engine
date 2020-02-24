import React from 'react';
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

import { theme } from '../theme';

import { StyledIcon } from './Icon';

// ************************************** //

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

// ************************************** //

export const Icon = props => {
  const { type } = props;

  const iconColor = theme.icons[type]
    ? theme.icons[type].color
    : theme.icons.default.color;

  // If the received icon type is out of range then the default icon will be assign to ActiveIcon.

  const CurrentActiveIcon = iconsReferenceObject[type]
    ? iconsReferenceObject[type]
    : iconsReferenceObject.default;

  return (
    <StyledIcon
      {...props}
      color={iconColor}
      fontSize="icon"
      width={18}
      height={18}
    >
      <CurrentActiveIcon />
    </StyledIcon>
  );
};
