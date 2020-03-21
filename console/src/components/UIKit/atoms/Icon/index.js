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
  FaCogs,
  FaExclamation,
  FaTimes,
  FaStar,
  FaTwitter,
  FaHeart,
  FaChevronRight,
  FaCompressArrowsAlt,
  FaExpand,
  FaEdit,
  FaChevronDown,
  FaSearch,
  FaSpinner,
  FaQuestionCircle,
} from 'react-icons/fa';

import { StyledIcon } from './Icon';

const iconReferenceMap = {
  success: FaCheckCircle,
  info: FaInfoCircle,
  warning: FaExclamationTriangle,
  error: FaExclamationCircle,
  graphiQL: FaFlask,
  database: FaDatabase,
  schema: FaPlug,
  event: FaCloud,
  settings: FaCog,
  question: FaQuestion,
  default: FaExclamationCircle,
  action: FaCogs,
  exclamation: FaExclamation,
  close: FaTimes,
  star: FaStar,
  twitter: FaTwitter,
  love: FaHeart,
  right: FaChevronRight,
  down: FaChevronDown,
  compress: FaCompressArrowsAlt,
  expand: FaExpand,
  edit: FaEdit,
  search: FaSearch,
  spinner: FaSpinner,
  questionCircle: FaQuestionCircle,
};

export const Icon = props => {
  const { type } = props;

  const CurrentActiveIcon = iconReferenceMap[type]
    ? iconReferenceMap[type]
    : iconReferenceMap.default;

  return <StyledIcon as={CurrentActiveIcon} {...props} aria-hidden="true" />;
};

Icon.defaultProps = {
  size: 18,
};
