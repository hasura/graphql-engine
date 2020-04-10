import styled, { css } from 'styled-components';
import {
  layout,
  space,
  color,
  border,
  typography,
  flexbox,
  LayoutProps,
  SpaceProps,
  TypographyProps,
  BorderProps,
  FlexboxProps,
  TextColorProps,
  BackgroundColorProps,
} from 'styled-system';
import { Theme } from '../../theme';

interface OmittedButtonProps
  extends Omit<
    React.ComponentPropsWithRef<'button'>,
    'type' | 'color' | 'disabled'
  > {}
const Button = ('button' as any) as React.FC<OmittedButtonProps>;

interface StyledButtonOwnProps
  extends LayoutProps,
    TextColorProps,
    BackgroundColorProps,
    TypographyProps,
    SpaceProps,
    FlexboxProps,
    BorderProps {
  boxShadowColor: string;
  type: keyof Theme['button'];
  disabled: boolean;
  opacity: string | undefined;
}

const hoverStyles = ({
  type,
  theme,
  disabled,
  boxShadowColor,
}: {
  type: keyof Theme['button'];
  theme: Theme;
  disabled: boolean;
  boxShadowColor: string;
}) => {
  if (type === 'secondary' && !disabled) {
    return css`
      &:hover {
        box-shadow: 0 2px 8px 0 ${boxShadowColor};
        background: ${theme.colors.black.secondary};
        color: ${theme.colors.white};
      }
    `;
  } else if (!disabled) {
    return css`
      &:hover {
        box-shadow: 0 2px 8px 0 ${boxShadowColor};
      }
    `;
  }
  return '';
};

export const StyledButton = styled(Button)<StyledButtonOwnProps>`
  appearance: button;
  
  cursor: ${({ disabled }) => !disabled && 'pointer'};
  &:disabled {
    cursor: not-allowed;
  }
  
  ${({ type, theme, disabled, boxShadowColor }) =>
    hoverStyles({ type, theme, disabled, boxShadowColor })}
  ${layout}
  ${space}
  ${typography}
  ${color}
  ${border}
  ${flexbox}
`;

export interface StyledButtonProps
  extends OmittedButtonProps,
    StyledButtonOwnProps {}
