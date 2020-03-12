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

import { theme } from '../../theme';
import { StyledIcon } from './Icon';

const iconReferenceMap = {
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

const iconWidth = 18;
const iconHeight = 18;

export const Icon = props => {
  const { type } = props;

  const { icon } = theme;

  const iconColor = icon[type] ? icon[type].color : icon.default.color;

  const CurrentActiveIcon = iconReferenceMap[type]
    ? iconReferenceMap[type]
    : iconReferenceMap.default;

  return (
    <StyledIcon
      {...props}
      color={iconColor}
      fontSize="icon"
      width={iconWidth}
      height={iconHeight}
    >
      <CurrentActiveIcon />
    </StyledIcon>
  );
};
