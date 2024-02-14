import * as AllReactIcons from 'react-icons/fa';
import { FaPlay } from 'react-icons/fa';

export function FaOptionIcon(
  react_icons_component_name: string | undefined | null
) {
  if (!react_icons_component_name) {
    return <FaPlay className="text-yellow-500 text-xl" />;
  }

  const DynamicIcon = (AllReactIcons as any)[react_icons_component_name];
  const icon = DynamicIcon ? (
    <DynamicIcon className="text-yellow-500 text-xl" />
  ) : (
    <FaPlay className="text-yellow-500 text-xl" />
  );
  return icon;
}
