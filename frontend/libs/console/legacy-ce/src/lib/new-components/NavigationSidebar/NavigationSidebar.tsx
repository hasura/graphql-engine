import clsx from 'clsx';
import React from 'react';
import { Link, RouteComponentProps } from 'react-router';
import { FaCheckCircle, FaTimesCircle } from 'react-icons/fa';
import { LocationDescriptor } from 'history';
import { sendTelemetryEvent } from '../../telemetry';

export type NavigationSidebarItem = {
  key: string;
  label: string;
  route: LocationDescriptor;
  status?: 'loading' | 'enabled' | 'disabled' | 'error' | 'none';
  dataTestVal?: string;
};

export type NavigationSidebarSection = {
  key: string;
  label: string;
  items: NavigationSidebarItem[];
};

export interface NavigationSidebarProps extends React.ComponentProps<'div'> {
  location: RouteComponentProps<unknown, unknown>['location'];
  sections: NavigationSidebarSection[];
}

export const NavigationSidebar = ({
  location,
  sections,
}: NavigationSidebarProps) => {
  return (
    <div className="flex flex-col w-full gap-4 bg-white p-3">
      {sections.map(section => (
        <div key={section.key} className="flex flex-col gap-2">
          <div className="text-md font-semibold uppercase text-muted">
            {section.label}
          </div>
          {section.items.map(item => (
            <Link
              key={item.key}
              className={clsx(
                'flex items-center justify-between gap-2 px-3 py-2 rounded-md cursor-pointer hover:bg-secondary-light focus:bg-secondary-light focus-visible:bg-secondary-light active:no-underline focus:no-underline'
              )}
              to={item.route}
              data-test={item.dataTestVal}
            >
              <div
                className={clsx(
                  'text-md',
                  location?.pathname?.includes(item.route.toString())
                    ? 'text-amber-500'
                    : 'text-muted'
                )}
                onClick={() => {
                  // Check if section.key is "schema-registry" and trigger telemetry event
                  if (item.key === 'schema-registry') {
                    sendTelemetryEvent({
                      type: 'CLICK_EVENT',
                      data: {
                        id: 'schema-registry-settings-btn',
                      },
                    });
                  }
                }}
              >
                {item.label}
              </div>
              <div
                className={clsx(
                  'rounded-full w-4 h-4',
                  item.status === 'loading' &&
                    'border border-muted border-t-0 border-b-0 border-l-0 animate-spin',
                  item.status === 'disabled' && 'border border-muted',
                  item.status === 'enabled' && 'text-emerald-600 self-baseline',
                  item.status === 'error' && 'text-red-600 self-baseline'
                )}
              >
                {item.status === 'enabled' ? (
                  <FaCheckCircle height="12px" width="12px" />
                ) : null}
                {item.status === 'error' ? (
                  <FaTimesCircle height="12px" width="12px" />
                ) : null}
              </div>
            </Link>
          ))}
        </div>
      ))}
    </div>
  );
};
