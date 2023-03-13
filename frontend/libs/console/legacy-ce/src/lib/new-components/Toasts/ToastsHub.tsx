import React from 'react';
import { resolveValue, useToaster } from 'react-hot-toast/headless';

export const ToastsHub = ({ className = '' }: { className?: string }) => {
  const { toasts, handlers } = useToaster();
  const { startPause, endPause, calculateOffset, updateHeight } = handlers;

  return (
    <div
      className={className}
      style={{
        position: 'fixed',
        top: 12,
        right: 10,
        zIndex: 9997,
      }}
      onMouseEnter={startPause}
      onMouseLeave={endPause}
    >
      {toasts.map(toast => {
        const offset = calculateOffset(toast, {
          reverseOrder: false,
          gutter: 10,
        });

        const ref = (el: HTMLDivElement) => {
          if (el && typeof toast.height !== 'number') {
            setTimeout(() => {
              const height = el.getBoundingClientRect().height;
              updateHeight(toast.id, height);
            });
          }
        };

        return (
          <div
            key={toast.id}
            ref={ref}
            style={{
              position: 'absolute',
              transition: 'all 0.3s ease-out',
              opacity: toast.visible ? 1 : 0,
              pointerEvents: toast.visible ? 'all' : 'none',
              transform: `translate(-100%, ${offset}px)`,
              zIndex: toast.visible ? 9999 : 9998,
            }}
            {...toast.ariaProps}
          >
            {resolveValue(toast.message, toast)}
          </div>
        );
      })}
    </div>
  );
};
