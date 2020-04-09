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
  FaGithub,
  FaExternalLinkAlt,
  FaDownload
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
  github: FaGithub,
  externalLink: FaExternalLinkAlt,
  download: FaDownload
};

export const Icon = props => {
  const { type } = props;

  const { icon } = theme;

  const iconColor = icon[type] ? icon[type].color : icon.default.color;

  const CurrentActiveIcon = iconReferenceMap[type]
    ? iconReferenceMap[type]
    : iconReferenceMap.default;

  return (
    <StyledIcon
      color={iconColor}
      as={CurrentActiveIcon}
      {...props}
    />
  );
};
