import {
  AlertMode,
  AlertParams,
  CommonAlertBase,
  ConfirmParams,
  PromptParams,
} from './types';

export type AlertComponentProps = AlertProps | ConfirmProps | PromptProps;

type AlertProps = PropsBase<'alert'> & AlertParams;
type ConfirmProps = PropsBase<'confirm'> & ConfirmParams;
type PromptProps = PropsBase<'prompt'> & PromptParams;

export type PropsBase<Mode extends AlertMode> = CommonAlertBase<Mode> & {
  open?: boolean;
  isLoading?: boolean;
  success?: boolean;
};
