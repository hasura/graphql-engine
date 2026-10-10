import { ReactSelectOptionType } from './types';

const FormatReactSelectOptionLabel = ({
  label,
  icon: Icon,
}: ReactSelectOptionType) => (
  <div>
    {Icon && (
      <span className="mr-2 -translate-y-0.5 inline-block">
        <Icon />
      </span>
    )}
    <span>{label}</span>
  </div>
);

export default FormatReactSelectOptionLabel;
