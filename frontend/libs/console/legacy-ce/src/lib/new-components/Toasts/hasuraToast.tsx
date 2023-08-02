import clsx from 'clsx';
import { deepmerge } from 'deepmerge-ts';
import { MouseEventHandler, PropsWithChildren, ReactNode } from 'react';
import { Toast, ToastOptions, toast } from 'react-hot-toast/headless';
import { FaTimesCircle } from 'react-icons/fa';
import { HtmlAnalyticsAttributes } from '../../features/Analytics';

export type ToastType = 'error' | 'success' | 'info' | 'warning';

const TOASTS_OPTIONS: ToastOptions = {
  duration: 3000,
  style: {
    borderRadius: '2px',
    borderTop: '2px solid',
    boxSizing: 'border-box',
    fontSize: '13px',
    margin: '0px',
    minWidth: 'min(400px, 75vw)',
    maxWidth: '75vw',
    padding: '10px',
    width: 'fit-content',
  },
};

const TOASTS_TYPES_OPTIONS: Record<ToastType, ToastOptions> = {
  error: {
    duration: Infinity,
    style: {
      backgroundColor: 'rgb(244, 233, 233)',
      borderColor: 'rgb(236, 61, 61)',
      boxShadow: 'rgb(236 61 61 / 90%) 0px 0px 1px',
      color: 'rgb(65, 47, 47)',
    },
    iconTheme: {
      primary: 'rgb(228, 190, 190)',
      secondary: 'rgb(244, 233, 233)',
    },
  },
  success: {
    duration: 5000,
    style: {
      backgroundColor: 'rgb(240, 245, 234)',
      borderColor: 'rgb(94, 164, 0)',
      boxShadow: 'rgb(94 164 0 / 90%) 0px 0px 1px',
      color: 'rgb(75, 88, 58)',
    },
    iconTheme: {
      primary: 'rgb(176, 202, 146)',
      secondary: 'rgb(240, 245, 234)',
    },
  },
  info: {
    duration: 5000,
    style: {
      backgroundColor: '#EAF1F4',
      borderColor: '#297393',
      boxShadow: 'rgb(54 156 199 / 90%) 0px 0px 1px',
      color: '#297393',
    },
    iconTheme: {
      primary: '#C3D8E1',
      secondary: '#C3D8E1',
    },
  },
  warning: {
    duration: Infinity,
    style: {
      backgroundColor: 'rgb(249, 246, 240)',
      borderColor: 'rgb(235, 173, 26)',
      boxShadow: 'rgb(235 173 23 / 90%) 0px 0px 1px',
      color: 'rgb(90, 83, 67)',
    },
    iconTheme: {
      primary: 'rgb(225, 207, 172)',
      secondary: 'rgb(249, 246, 240)',
    },
  },
};

export type ToastProps = PropsWithChildren<{
  type?: ToastType;
  title?: string;
  message?: string;
  button?: {
    label: ReactNode;
    onClick: MouseEventHandler<HTMLButtonElement>;
    dataAttributes?: HtmlAnalyticsAttributes;
  };
  onRemove?: () => void;
  toastOptions?: ToastOptions;
}>;

export const dismissAllToasts = () => {
  toast.dismiss();
};

export const hasuraToast = ({
  type = 'info',
  title,
  message,
  button,
  onRemove = () => {},
  toastOptions = {},
  children,
}: ToastProps) => {
  const handleOnRemove = (t: Toast) => () => {
    toast.dismiss(t.id);
    onRemove();
  };

  return toast.custom(
    t => (
      <div
        className={clsx(
          'font-sans flex between items-start w-full',
          t.visible ? 'animate-notificationOpen' : 'animate-notificationClose'
        )}
        style={{
          ...t.style,
        }}
        data-testid="notification"
        data-notificationtype={type}
        data-toastid={t.id}
      >
        <div className="flex flex-col flex-grow items-stretch">
          {title ? (
            <div
              className="font-semibold text-base w-full"
              style={{
                color: TOASTS_TYPES_OPTIONS[type].style?.borderColor,
              }}
            >
              {title}
            </div>
          ) : null}
          <div className="w-full whitespace-pre-line">{message}</div>
          <div className="w-full">{children}</div>
          {button ? (
            <div>
              <button
                className="mt-2 text-white h-btn px-sm justify-center inline-flex items-center text-sm font-sans font-semibold bg-gradient-to-t border rounded shadow-sm focus-visible:outline-none focus-visible:bg-gradient-to-t focus-visible:ring-2 focus-visible:ring-offset-2 focus-visible:ring-yellow-400 disabled:opacity-60"
                style={{
                  backgroundColor:
                    TOASTS_TYPES_OPTIONS[type].style?.borderColor,
                }}
                onClick={button.onClick}
                {...button.dataAttributes}
              >
                {button.label}
              </button>
            </div>
          ) : null}
        </div>
        <button onClick={handleOnRemove(t)} className="p-1">
          <FaTimesCircle
            color={TOASTS_TYPES_OPTIONS[type].iconTheme?.primary}
          />
        </button>
      </div>
    ),
    deepmerge(
      deepmerge(TOASTS_OPTIONS, TOASTS_TYPES_OPTIONS[type]),
      toastOptions
    )
  );
};
