import clsx from 'clsx';
import { CgDockBottom } from 'react-icons/cg';
import { Button } from '../../Button';
import { FormDebug } from './FormDebug';
import React from 'react';
import { useSessionStoreState } from '../../../utils/sessionStorage';
import { consoleDevToolsEnabled } from '../../../utils/console-dev-tools/consoleDevToolsEnabled';
/**
 *
 * Renders the FormDebug component in a window that stays fixed to top/bottom right of your window
 *
 */
export function FormDebugWindow() {
  const [position, setPosition] = useSessionStoreState('formDebug.position');

  const devToolsEnabled = consoleDevToolsEnabled();

  if (!devToolsEnabled) return null;

  return (
    <div
      className={clsx(
        'flex flex-col gap-2 fixed right-0 p-4 bg-opacity-90 bg-slate-200 rounded shadow-sm m-2 max-w-xl overflow-auto max-h-[320px] w-[320px] z-50',
        position === 'top' ? 'top-0' : 'bottom-0'
      )}
    >
      <div className="flex justify-between items-center">
        <div className="font-semibold text-mg">Form Debug:</div>
        <div className="flex justify-end">
          <Button
            className="self-center rounded-tr-none rounded-br-none"
            size="sm"
            disabled={position === 'top'}
            onClick={() => setPosition('top')}
            icon={<CgDockBottom className="rotate-180" />}
          />
          <Button
            disabled={position === 'bottom'}
            className="self-center rounded-tl-none rounded-bl-none"
            size="sm"
            onClick={() => setPosition('bottom')}
            icon={<CgDockBottom />}
          />
        </div>
      </div>
      <FormDebug />
    </div>
  );
}
