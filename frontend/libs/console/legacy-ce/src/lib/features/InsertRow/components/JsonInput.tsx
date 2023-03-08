import {
  ExpandableTextInputProps,
  ExpandableTextInput,
} from './ExpandableTextInput';

export const JsonInput: React.VFC<ExpandableTextInputProps> = props => (
  <ExpandableTextInput {...props} mode="json" theme="github" />
);
