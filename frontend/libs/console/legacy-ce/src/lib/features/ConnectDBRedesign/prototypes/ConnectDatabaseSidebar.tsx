import * as ScrollArea from '@radix-ui/react-scroll-area';
import clsx from 'clsx';
import React from 'react';
import { z } from 'zod';
import { Button } from '../../../new-components/Button';
import { Collapsible } from '../../../new-components/Collapsible';
import { InputField, useConsoleForm } from '../../../new-components/Form';
import { DriverInfo } from '../../DataSource';
import { useAddAgent } from '../../ManageAgents/hooks';
import { agentPaths } from '../components/SetupConnector/hooks/useSuperConnectorAgents';
import { ConnectDatabaseProps } from '../ConnectDatabase';
import dbLogos from '../graphics/db-logos';
import { useDatabaseConnectDrivers } from '../hooks';
import { SuperConnectorDrivers } from '../../hasura-metadata-types';

export const ConnectDatabaseSidebar = (props: ConnectDatabaseProps) => {
  const { consoleType } = props;
  const showEnterpriseDrivers = consoleType !== 'oss';
  const { allDrivers, availableDrivers } = useDatabaseConnectDrivers({
    showEnterpriseDrivers: showEnterpriseDrivers,
  });

  const { Form, methods } = useConsoleForm({
    schema: z.object({ search: z.string() }),
  });
  const searchQuery = methods.watch('search');

  const nativeDrivers = React.useMemo(
    () =>
      allDrivers.filter(
        d =>
          d.native &&
          (!searchQuery?.trim() ||
            d.displayName.toLowerCase().includes(searchQuery.toLowerCase()))
      ),
    [searchQuery, allDrivers]
  );
  const dataConnectors = React.useMemo(
    () =>
      allDrivers.filter(
        d =>
          !d.native &&
          (!d.enterprise || showEnterpriseDrivers) &&
          (!searchQuery?.trim() ||
            d.displayName.toLowerCase().includes(searchQuery.toLowerCase()))
      ),
    [searchQuery, allDrivers]
  );
  return (
    <div>
      <Form onSubmit={() => {}}>
        <InputField
          placeholder="Search Native Drivers & Data Connectors"
          name="search"
          clearButton
        />
      </Form>
      <ListContent
        {...props}
        //allowNeonConnect={allowNeonConnect}
        contentClassName="h-[500px]"
        availableDrivers={availableDrivers ?? []}
        items={nativeDrivers}
        title={'Built-In Drivers'}
      />
      <ListContent
        {...props}
        contentClassName="h-[300px]"
        availableDrivers={availableDrivers ?? []}
        items={dataConnectors}
        title={'Hasura Data Connectors'}
      />
    </div>
  );
};

const ScrollContainer: React.FC<{ className?: string }> = ({
  children,
  className,
}) => (
  <ScrollArea.Root
    className={clsx('w-full h-[400px] overflow-hidden bg-white', className)}
  >
    <ScrollArea.Viewport className="w-full h-full">
      {children}
    </ScrollArea.Viewport>
    <ScrollArea.Scrollbar
      className="flex select-none touch-none p-0.5 bg-blackA6 transition-colors duration-[160ms] ease-out hover:bg-blackA8 data-[orientation=vertical]:w-2.5 data-[orientation=horizontal]:flex-col data-[orientation=horizontal]:h-2.5"
      orientation="vertical"
    >
      <ScrollArea.Thumb className="flex-1 bg-mauve10 rounded-[10px] relative before:content-[''] before:absolute before:top-1/2 before:left-1/2 before:-translate-x-1/2 before:-translate-y-1/2 before:w-full before:h-full before:min-w-[44px] before:min-h-[44px]" />
    </ScrollArea.Scrollbar>
    <ScrollArea.Scrollbar
      className="flex select-none touch-none p-0.5 bg-blackA6 transition-colors duration-[160ms] ease-out hover:bg-blackA8 data-[orientation=vertical]:w-2.5 data-[orientation=horizontal]:flex-col data-[orientation=horizontal]:h-2.5"
      orientation="horizontal"
    >
      <ScrollArea.Thumb className="flex-1 bg-mauve10 rounded-[10px] relative before:content-[''] before:absolute before:top-1/2 before:left-1/2 before:-translate-x-1/2 before:-translate-y-1/2 before:w-full before:h-full before:min-w-[44px] before:min-h-[44px]" />
    </ScrollArea.Scrollbar>
    <ScrollArea.Corner className="bg-blackA8" />
  </ScrollArea.Root>
);

type ListContentProps = {
  items: DriverInfo[];
  title: string;
  availableDrivers: DriverInfo[];
  contentClassName?: string;
} & ConnectDatabaseProps;

const ListContent = ({
  items,
  title,
  availableDrivers,
  contentClassName,
  consoleType,
}: ListContentProps) => {
  const { addAgent } = useAddAgent();
  return (
    <Collapsible
      defaultOpen
      disableContentStyles
      animationSpeed="fast"
      triggerClassName="bg-slate-100 w-full px-2 py-1"
      doNotWrapChildren
      triggerChildren={
        <div className="flex flex-row justify-between w-full">
          <div className="font-semibold">{title}</div>
          <div className="flex items-center bg-blue-500 opacity-80 rounded-full text-sm px-2 text-white">
            {items.length}
          </div>
        </div>
      }
    >
      <ScrollContainer className={clsx(contentClassName)}>
        {items.map(d => (
          <div className="flex-wrap flex flex-row items-center gap-4 p-4 transition-all duration-200 cursor-pointer group hover:bg-blue-50">
            <img
              className="h-12 w-12 object-contain"
              alt={`${d.displayName} logo`}
              src={dbLogos[d.name] ?? dbLogos.default}
            />
            <div className="font-bold text-muted transition-all duration-200 group-hover:text-slate-900 flex-grow items-center flex flex-row justify-between">
              {d.displayName}
              {consoleType !== 'cloud' &&
                !availableDrivers.some(driver => driver.name === d.name) && (
                  <Button
                    mode="primary"
                    size="sm"
                    onClick={() => {
                      const path = agentPaths[d.name as SuperConnectorDrivers];
                      if (path) {
                        addAgent({
                          name: d.name,
                          url: `http://host.docker.internal:5000${path}`,
                        }).then(() => {});
                      }
                    }}
                  >
                    Install
                  </Button>
                )}
            </div>
          </div>
        ))}
      </ScrollContainer>
    </Collapsible>
  );
};
