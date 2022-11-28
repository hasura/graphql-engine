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
  FaTrashAlt,
  FaPencilAlt,
  FaPlus,
  FaEye,
  FaUserSecret,
  FaRegLightbulb,
  FaSort,
  FaCaretUp,
  FaCaretDown,
  FaCaretRight,
  FaCaretLeft,
  FaRegClone,
  FaRegCaretSquareRight,
  FaCopy,
  FaExternalLinkAlt,
  FaTable,
  FaFilter,
  FaWrench,
  FaRegPaperPlane,
  FaCodeBranch,
  FaGithub,
  FaDownload,
} from 'react-icons/fa';

import { Theme } from '../../theme';
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
  delete: FaTrashAlt,
  add: FaPlus,
  eye: FaEye,
  secret: FaUserSecret,
  lightBulb: FaRegLightbulb,
  sort: FaSort,
  caretUp: FaCaretUp,
  caretDown: FaCaretDown,
  caretRight: FaCaretRight,
  caretLeft: FaCaretLeft,
  clone: FaRegClone,
  copy: FaCopy,
  link: FaExternalLinkAlt,
  table: FaTable,
  filter: FaFilter,
  wrench: FaWrench,
  send: FaRegPaperPlane,
  fork: FaCodeBranch,
  github: FaGithub,
  download: FaDownload,
};

export type IconProps = {
  pointer?: boolean;
  size?: number;
  type: keyof Theme['icon'];
};

export const Icon: React.FC<IconProps> = props => {
  const { type } = props;
  const CurrentActiveIcon = iconReferenceMap[type]
    ? iconReferenceMap[type]
    : iconReferenceMap.default;
  return <StyledIcon as={CurrentActiveIcon} {...props} aria-hidden="true" />;
};
Icon.defaultProps = {
  size: 14,
};
