import React from 'react';
import {
  FaBolt,
  FaCogs,
  FaDatabase,
  FaLink,
  FaLock,
  FaMobileAlt,
  FaPlug,
  FaProjectDiagram,
  FaServer,
} from 'react-icons/fa';

export const QueryCollectionOperationsEmptyState = () => {
  return (
    <div className="my-sm relative">
      <div className="flex w-max bg-gray-200 border border-gray-300 rounded pt-8 p-md">
        <div className="flex items-center relative pr-6">
          <span className="absolute -top-3.5 left-0 text-sm font-semibold text-gray-400 uppercase tracking-wider">
            Sources
          </span>

          <div className="absolute h-full right-0 border-r border-gray-400" />
          <div className="absolute w-4 border-t border-gray-400 top-0 right-0" />
          <div className="absolute w-4 border-b border-gray-400 bottom-0 right-0" />

          <div className="space-y-sm py-sm">
            <div className="flex items-center justify-end">
              <div className="relative bg-transparent rounded border border-gray-400 text-gray-400 overflow-hidden p-sm w-48">
                <div className="flex items-center">
                  <FaCogs className="mr-1.5" />
                  <span>REST Endpoints</span>
                </div>
              </div>
            </div>

            <div className="flex items-center justify-end">
              <div className="relative bg-transparent rounded border border-gray-400 text-gray-400 overflow-hidden p-sm w-48">
                <div className="flex items-center">
                  <FaDatabase className="mr-1.5" />
                  <span>Database</span>
                </div>
              </div>
            </div>

            <div className="flex items-center justify-end">
              <div className="relative bg-transparent rounded border border-gray-400 text-gray-400 overflow-hidden p-sm w-48">
                <div className="flex items-center">
                  <FaPlug className="mr-1.5" />
                  <span>GraphQL Services</span>
                </div>
              </div>
            </div>
          </div>
        </div>

        <div className="flex items-center">
          <div className="w-4 border-t border-gray-400" />
          <span className="flex items-center px-xs text-sm text-gray-400">
            <FaLink className="mr-1.5" />
            Connected
          </span>
          <div className="w-4 border-t border-gray-400" />
          <svg
            className="text-gray-400 w-4 h-full -ml-1.5"
            stroke="currentColor"
            fill="currentColor"
            strokeWidth="0"
            viewBox="0 0 16 16"
            height="1em"
            width="1em"
            xmlns="http://www.w3.org/2000/svg"
          >
            <path d="M5.56 14L5 13.587V2.393L5.54 2 11 7.627v.827L5.56 14z" />
          </svg>
        </div>

        <div className="mt-20">
          <div className="bg-white rounded shadow w-48 hover:shadow-md">
            <div className="relative p-sm flex items-center">
              <div className="absolute -mt-[8px] top-1/2 -right-[6px] animate-ping rounded-full w-6 h-6 bg-amber-500 z-10" />
              <div className="absolute flex items-center justify-center -mt-[8px] top-1/2 -right-[6px] rounded-full w-6 h-6 bg-amber-500 z-10">
                <FaLock color="white" className="w-2 h-2" />
              </div>
              <img
                alt="hasura"
                className="w-3 h-3 mr-1.5"
                src="data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iODEiIGhlaWdodD0iODQiIGZpbGw9Im5vbmUiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI+PGcgY2xpcC1wYXRoPSJ1cmwoI2EpIiBmaWxsPSIjMDAwIj48cGF0aCBkPSJNNzkuNzE5IDI4LjYwMmMyLjQwMy03LjUyOS45NTktMjIuNTY2LTMuNzAzLTI4LjExNC0uNjA5LS43MjYtMS43NTQtLjYyMi0yLjI1OS4xNzZsLTUuNzQ1IDkuMDY0YTQuNDAzIDQuNDAzIDAgMCAxLTUuOS45NjQgMzkuMzI0IDM5LjMyNCAwIDAgMC0yMS42OC02LjQ4MSAzOS4zMjQgMzkuMzI0IDAgMCAwLTIxLjY4IDYuNDgxIDQuNDE0IDQuNDE0IDAgMCAxLTUuOS0uOTY0TDcuMTA3LjY2NEM2LjYwMi0uMTM0IDUuNDU3LS4yMzggNC44NS40ODguMTg3IDYuMDM2LTEuMjU3IDIxLjA3MyAxLjE0NiAyOC42MDJjLjc5NCAyLjUgMS4wMiA1LjE0NC41NDYgNy43MjYtLjQ2NCAyLjU1MS0uOTM4IDUuNjQxLS45MzggNy43NzhDLjc1NCA2Ni4xMzIgMTguNTI1IDg0IDQwLjQzMiA4NCA2Mi4zNSA4NCA4MC4xMSA2Ni4xNDIgODAuMTEgNDQuMTA1YzAtMi4xNDctLjQ2NC01LjIyNy0uOTM4LTcuNzc4LS40NzQtMi41ODItLjI0OC01LjIyNy41NDctNy43MjZabS0zOS4yODcgNDYuNDhjLTE2LjkzNiAwLTMwLjcxNS0xMy44NTUtMzAuNzE1LTMwLjg4MyAwLS41Ni4wMi0xLjExLjA0MS0xLjY2LjYxOS0xMS42MDQgNy42MjItMjEuNTI4IDE3LjU0NC0yNi4yNTdhMzAuMyAzMC4zIDAgMCAxIDEzLjEzLTIuOTY2YzQuNjkzIDAgOS4xNDkgMS4wNjggMTMuMTQgMi45NzYgOS45MjIgNC43MyAxNi45MjYgMTQuNjU0IDE3LjU0NSAyNi4yNDguMDMuNTUuMDQgMS4wOTkuMDQgMS42NTktLjAxIDE3LjAyOC0xMy43OSAzMC44ODMtMzAuNzI1IDMwLjg4M1oiLz48cGF0aCBkPSJtNTMuNzM3IDU2LjA4My03Ljg0OS0xMy42NzgtNi43MzUtMTEuNDA4YS44NzQuODc0IDAgMCAwLS43NjMtLjQzNmgtNi40MzZBLjg4My44ODMgMCAwIDAgMzEuMiAzMS45bDYuNDM2IDEwLjg5LTguNjQzIDEzLjI1MmEuOTAzLjkwMyAwIDAgMC0uMDQyLjkxM2MuMTU1LjI5LjQ1NC40NjcuNzc0LjQ2N2g2LjQ3N2MuMyAwIC41NzgtLjE1Ni43NDMtLjQwNWw0LjY3Mi03LjM0MiA0LjE4OCA3LjNjLjE1NC4yOC40NTMuNDQ3Ljc2My40NDdoNi4zODRjLjMyIDAgLjYwOS0uMTY2Ljc2NC0uNDQ2YS44MjIuODIyIDAgMCAwIC4wMi0uODkyWiIvPjwvZz48ZGVmcz48Y2xpcFBhdGggaWQ9ImEiPjxwYXRoIGZpbGw9IiNmZmYiIGQ9Ik0wIDBoODF2ODRIMHoiLz48L2NsaXBQYXRoPjwvZGVmcz48L3N2Zz4="
              />
              <span className="font-semibold">Hasura</span>
            </div>
            <div className="relative">
              <span className="flex items-center border-t border-gray-200 bg-gray-100 text-sm rounded-b py-1.5 px-sm">
                <FaLock className="mr-1.5" />
                <span>Allow List Operations</span>
              </span>
            </div>
          </div>
        </div>

        <div className="flex items-center">
          <div className="w-4 border-t border-gray-400" />
          <svg
            className="text-gray-400 w-4 h-full -ml-1.5"
            stroke="currentColor"
            fill="currentColor"
            strokeWidth="0"
            viewBox="0 0 16 16"
            height="1em"
            width="1em"
            xmlns="http://www.w3.org/2000/svg"
          >
            <path d="M5.56 14L5 13.587V2.393L5.54 2 11 7.627v.827L5.56 14z" />
          </svg>
          <span className="flex items-center px-xs text-sm text-gray-400">
            <FaBolt className="mr-1.5" />
            Powering
          </span>
          <div className="w-4 border-t border-gray-400" />
        </div>

        <div className="flex items-center relative pl-6">
          <span className="absolute -top-3.5 right-0 text-sm font-semibold text-gray-400 uppercase tracking-wider">
            Consumers
          </span>

          <div className="absolute h-full left-0 border-r border-gray-400" />
          <div className="absolute w-4 border-t border-gray-400 top-0 left-0" />
          <div className="absolute w-4 border-b border-gray-400 bottom-0 left-0" />

          <div className="space-y-sm py-sm">
            <div className="flex items-center justify-end">
              <div className="relative bg-transparent rounded border border-gray-400 text-gray-400 overflow-hidden p-sm w-48">
                <div className="flex items-center">
                  <FaMobileAlt className="mr-1.5" />
                  <span>Apps</span>
                </div>
              </div>
            </div>

            <div className="flex items-center justify-end">
              <div className="relative bg-transparent rounded border border-gray-400 text-gray-400 overflow-hidden p-sm w-48">
                <div className="flex items-center">
                  <FaServer className="mr-1.5" />
                  <span>Data Platforms</span>
                </div>
              </div>
            </div>

            <div className="flex items-center justify-end">
              <div className="relative bg-transparent rounded border border-gray-400 text-gray-400 overflow-hidden p-sm w-48">
                <div className="flex items-center">
                  <FaProjectDiagram className="mr-1.5" />
                  <span>Other Services</span>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};
