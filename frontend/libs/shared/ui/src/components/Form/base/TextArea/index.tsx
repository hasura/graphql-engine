import React, { Ref } from 'react';
import {
  TextArea as ThemeTextArea,
  TextAreaProps as ThemeTextAreaProps,
} from '@radix-ui/themes';

export type TextAreaProps = Omit<ThemeTextAreaProps, 'ref'> & {
  invalid?: boolean;
};

export const TextArea = React.forwardRef<HTMLTextAreaElement, TextAreaProps>(
  ({ invalid, color, ...props }, ref) => {
    return (
      <ThemeTextArea
        {...props}
        ref={ref as Ref<HTMLTextAreaElement>}
        color={invalid ? 'red' : color}
      />
    );
  },
);
