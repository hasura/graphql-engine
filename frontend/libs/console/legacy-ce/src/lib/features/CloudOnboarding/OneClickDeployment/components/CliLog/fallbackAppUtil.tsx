import * as AllReactIcons from 'react-icons/fa';
import { FaPlay } from 'react-icons/fa';
import { FallbackApp } from '../../types';

export const transformFallbackAppToLinkButtonProps = ({
  name,
  href,
  react_icons_component_name,
}: FallbackApp) => {
  const DynamicIcon = (AllReactIcons as any)[react_icons_component_name];
  const icon = DynamicIcon ?? FaPlay;
  return {
    url: href,
    buttonText: name,
    icon,
  };
};
