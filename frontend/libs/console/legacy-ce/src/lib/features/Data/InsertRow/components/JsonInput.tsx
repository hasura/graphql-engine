import {
  ExpandableTextInputProps,
  ExpandableTextInput,
} from './ExpandableTextInput';

export const JsonInput: React.FC<ExpandableTextInputProps> = (props) => (
  <ExpandableTextInput {...props} mode="json" />
);
