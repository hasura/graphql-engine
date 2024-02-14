import React from 'react';
import clsx from 'clsx';
import * as DropdownMenu from '@radix-ui/react-dropdown-menu';
import { HasuraLogoIcon } from '../../new-components/HasuraLogo';
import { Switch } from '../../new-components/Switch';
import { consoleDevToolsEnabled } from './consoleDevToolsEnabled';

export interface ConsoleDevToolsProps {}

export function ConsoleDevTools(props: ConsoleDevToolsProps) {
  const enabled = consoleDevToolsEnabled();

  if (!enabled) return null;

  return <ConsoleDevToolsInner {...props} />;
}

export function ConsoleDevToolsInner(props: ConsoleDevToolsProps) {
  const [open, setOpen] = React.useState(false);
  const [activateBootstrapGlobally, setActivateBootstrapGlobally] =
    React.useState(false);
  const [displayBootstrapOverlay, setDisplayBootstrapOverlay] =
    React.useState(false);

  const handleMouseMove = (evt: MouseEvent) => {
    setTimeout(() => {
      document.documentElement.style.setProperty(
        '--mouse-x',
        evt.clientX + 'px'
      );
      document.documentElement.style.setProperty(
        '--mouse-y',
        evt.clientY + 'px'
      );
    });
  };

  React.useEffect(() => {
    document.body.classList.add('!overflow-y-scroll');
    document.body.classList.add('!m-0');

    /**
     * Bootstrap overlay will cover part of the UI where bootstrap is jailed.
     * It displays a hole to clearly see through and allows to interact what is around the cursor behind the overlay.
     * The overlay is a ::before pseudo-element covering the bootstrap-jail element, it has pointer-events: none to allow
     * to interact through it.
     * The hole is a radial gradient with a transparent center.
     * The whole ::before element is translated to the cursor position thanks to CSS variables for cursor position and element offsets.
     * The CSS variables are updated on mousemove for cursor position and every 100ms for elements offsets.
     */
    const sheet = document.createElement('style');
    sheet.innerHTML = `
      .bootstrap-debug .bootstrap-jail,
      .bootstrap-debug.bootstrap-jail {
        position: relative;
        overflow: hidden;
      }

      .bootstrap-debug .bootstrap-jail::before,
      .bootstrap-debug.bootstrap-jail::before {
        pointer-events: none;
        background-color: #730cf64d;
        border: 1px solid #730cf6;
        content: '';
        display: block;
        position: absolute;
        height: 10000px;
        width: 10000px;
        top: 0;
        right: 0;
        bottom: 0;
        left: 0;
        z-index: 9999999;
      }

      .bootstrap-debug .bootstrap-jail:hover::before,
      .bootstrap-debug.bootstrap-jail:hover::before {
        background-color: transparent;
        background-image: radial-gradient(
          circle at center,
          rgba(255, 255, 255, 0) 56px,
          rgba(115, 12, 246, 0.3) 57px,
          #730cf64d 100%
        );
        background-size: 100% 100%;
        background-repeat: no-repeat;
        background-position: center center;
        transform: translate(
          calc(var(--mouse-x) - var(--rect-left) - 50%),
          calc(var(--mouse-y) - var(--rect-top) - 50%)
        );
      }
        `;
    document.body.appendChild(sheet);

    const intervalId = setInterval(() => {
      setTimeout(() => {
        document
          .querySelectorAll('.bootstrap-jail')
          .forEach((element: Element) => {
            (element as HTMLElement).style.setProperty(
              '--rect-left',
              element.getBoundingClientRect().left + 'px'
            );
            (element as HTMLElement).style.setProperty(
              '--rect-top',
              element.getBoundingClientRect().top + 'px'
            );
          });
      });
    }, 100);

    document.addEventListener('mousemove', handleMouseMove);

    return () => {
      window.removeEventListener('mousemove', handleMouseMove);
      clearInterval(intervalId);
    };
  }, []);

  React.useEffect(() => {
    if (document.body.classList.contains('bootstrap-jail')) {
      document.body.classList.remove('bootstrap-jail');
    } else if (activateBootstrapGlobally) {
      document.body.classList.add('bootstrap-jail');
    }
  }, [activateBootstrapGlobally]);

  React.useEffect(() => {
    if (document.body.classList.contains('bootstrap-debug')) {
      document.body.classList.remove('bootstrap-debug');
    } else if (displayBootstrapOverlay) {
      document.body.classList.add('bootstrap-debug');
    }
  }, [displayBootstrapOverlay]);

  return (
    <div className="fixed bottom-4 left-1/2 translate-x-1/2 z-[9999999]">
      <DropdownMenu.Root open={open} onOpenChange={setOpen}>
        <DropdownMenu.Trigger>
          <div
            className={clsx(
              'flex align-center justify-center p-3 rounded-full bg-slate-100 cursor-pointer shadow hover:shadow-md hover:rotate-6 transition-transform shadow-secondary-dark hover:shadow-secondary-dark',
              open && 'shadow-md rotate-6'
            )}
          >
            <HasuraLogoIcon mode="secondary" className="!m-0" />
          </div>
        </DropdownMenu.Trigger>
        <DropdownMenu.Portal>
          <DropdownMenu.Content
            side="top"
            sideOffset={16}
            align="center"
            className="z-[9999999] w-max rounded shadow-sm bg-slate-100 shadow-secondary-dark hover:shadow-secondary-dark"
          >
            <DropdownMenu.Item
              className="flex align-center gap-2 p-3"
              onSelect={event => event.preventDefault()}
            >
              <Switch
                checked={activateBootstrapGlobally}
                onCheckedChange={setActivateBootstrapGlobally}
              />{' '}
              Activate Boostrap Globally
            </DropdownMenu.Item>
            <DropdownMenu.Separator />
            <DropdownMenu.Item
              className="flex align-center gap-2 p-3"
              onSelect={event => event.preventDefault()}
            >
              <Switch
                checked={displayBootstrapOverlay}
                onCheckedChange={setDisplayBootstrapOverlay}
              />{' '}
              Display Bootstrap Overlay
            </DropdownMenu.Item>
          </DropdownMenu.Content>
        </DropdownMenu.Portal>
      </DropdownMenu.Root>
    </div>
  );
}

export default ConsoleDevTools;
