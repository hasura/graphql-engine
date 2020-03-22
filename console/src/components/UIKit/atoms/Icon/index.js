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
  FaPause,
  FaPlay,
  FaCheck,
  FaRedoAlt,
  FaChevronUp,
  FaTrash,
  FaPencilAlt,
  FaPlus,
  FaEye,
  FaUserSecret,
  FaRegLightbulb,
  FaSort,
  FaCaretUp,
  FaCaretDown,
  FaRegClone,
  FaRegCaretSquareRight,
  FaCopy,
  FaExternalLinkAlt,
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
  up: FaChevronUp,
  compress: FaCompressArrowsAlt,
  expand: FaExpand,
  edit: FaEdit,
  pencil: FaPencilAlt,
  search: FaSearch,
  spinner: FaSpinner,
  questionCircle: FaQuestionCircle,
  pause: FaPause,
  play: FaPlay,
  playBox: FaRegCaretSquareRight,
  check: FaCheck,
  reload: FaRedoAlt,
  delete: FaTrash,
  add: FaPlus,
  eye: FaEye,
  secret: FaUserSecret,
  lightBulb: FaRegLightbulb,
  sort: FaSort,
  caretUp: FaCaretUp,
  caretDown: FaCaretDown,
  clone: FaRegClone,
  copy: FaCopy,
  link: FaExternalLinkAlt,
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
