import React from 'react';
import {
  FaDatabase,
  FaPlug,
  FaCogs,
  FaLink,
  FaBolt,
  FaCaretRight,
  FaMobileAlt,
  FaServer,
  FaShareAlt,
} from 'react-icons/fa';
import { HasuraLogoIcon } from '@/new-components/HasuraLogo';
import './OnboardingAnimation.css';

const commonStyles = {
  consumerList:
    'relative bg-white rounded border border-amber-500 shadow shadow-amber-500/50 overflow-hidden p-sm w-48',
  featuresList:
    'opacity-0 group flex items-center border-t border-gray-200 bg-gray-100 text-gray-400 text-sm px-sm',
};

export function OnboardingAnimation() {
  return (
    <div className="flex justify-center overflow-auto bg-gray-200 border border-gray-300 rounded-b p-md mb-md">
      <div className="flex items-center relative pr-6">
        <div className="animate_500 opacity-0 absolute top-0 left-0 text-sm font-semibold text-muted uppercase tracking-wider">
          Sources
        </div>

        <div className="animate_2500 opacity-0 absolute h-full right-0 border-r border-gray-400" />
        <div className="animate_2500 opacity-0 absolute w-4 border-t border-gray-400 top-0 right-0" />
        <div className="animate_2500 opacity-0 absolute w-4 border-b border-gray-400 bottom-0 right-0" />

        <div className="space-y-md">
          <div className="animate_1500 opacity-0 flex items-center justify-end">
            <div className="relative bg-transparent rounded border border-gray-400 text-gray-400 overflow-hidden p-sm w-48">
              <div className="flex items-center">
                <FaCogs className="fill-current w-4 h-4 mr-1.5" />
                <div className="font-semibold">REST Endpoints</div>
              </div>
            </div>
          </div>

          <div className="animate_1000 opacity-0 flex items-center justify-end">
            <div className="relative bg-white rounded shadow overflow-hidden border shadow p-sm w-48 hover:shadow-md">
              <div className="flex items-center">
                <FaDatabase className="fill-current w-4 h-4 mr-1.5" />
                <div className="font-semibold">Databases</div>
              </div>
            </div>
          </div>

          <div className="animate_2000 opacity-0 flex items-center justify-end">
            <div className="relative bg-transparent rounded border border-gray-400 text-gray-400 overflow-hidden p-sm w-48">
              <div className="flex items-center">
                <FaPlug className="fill-current w-4 h-4 mr-1.5" />
                <div className="font-semibold">GraphQL Services</div>
              </div>
            </div>
          </div>
        </div>
      </div>

      <div className="animate_2500 opacity-0 flex items-center">
        <div className="-mt-2.5 w-4 border-t border-gray-400" />
        <div className="-mt-2.5 flex items-center px-xs text-sm text-muted">
          <FaLink className="mr-1 w-3" />
          Connected
        </div>
        <div className="-mt-2.5 w-4 border-t border-gray-400" />
        <FaCaretRight className="-mt-2.5 w-2 mr-1 text-gray-400 h-full -ml-1.5" />
      </div>

      <div className="flex items-center">
        <div className="animate_3000 opacity-0 bg-white rounded shadow w-48 hover:shadow-md">
          <div className="p-sm flex items-center">
            <HasuraLogoIcon size="sm" />
            <div className="font-semibold">Hasura</div>
          </div>
          <div className="relative">
            <div className={`animate_3500 ${commonStyles.featuresList} py-1.5`}>
              <div>Authentication</div>
            </div>
            <div
              className={`animate_4000 ${commonStyles.featuresList} pt-1.5 pb-6`}
            >
              <div>Permissions</div>
            </div>

            <div className="animate_4500 opacity-0 group flex items-center bg-gray-100 text-gray-400 text-sm px-sm pb-1.5 pt-6 ">
              <div>REST API</div>
            </div>
            <div className="animate_6500 -mt-[2rem] absolute top-[30%] -translate-y-1/2 font-semibold scale-105 w-48 opacity-0 group flex items-center text-sm px-sm rounded shadow border-gray-200 bg-white py-1.5">
              <div className="absolute top-2.5 right-2.5 rounded-full w-3 h-3 bg-[#f59e0b] animate-ping" />
              <div className="absolute top-2.5 right-2.5 rounded-full w-3 h-3 bg-[#f59e0b]" />
              <div>GraphQL API</div>
            </div>
            <div className={`animate_5000 ${commonStyles.featuresList} py-1.5`}>
              <div>Relationships</div>
            </div>
            <div className={`animate_5500 ${commonStyles.featuresList} py-1.5`}>
              <div>Caching</div>
            </div>
            <div
              className={`animate_6000 ${commonStyles.featuresList} py-1.5  rounded-b`}
            >
              <div>Observability</div>
            </div>
          </div>
        </div>
      </div>

      <div className="animate_7000 opacity-0 flex items-center">
        <div className="-mt-2.5 w-2 ml-1.5 border-t border-gray-400" />
        <FaCaretRight className="-mt-2.5 text-gray-400 w-2 h-full" />
        <div className="-mt-2.5 flex items-center px-xs text-sm text-muted">
          <FaBolt className="text-muted mr-1 w-3" />
          Powering
        </div>
        <div className="-mt-2.5 w-4 border-t border-gray-400" />
      </div>

      <div className="flex items-center relative pl-6">
        <div className="animate_7500 opacity-0 absolute top-0 right-0 text-sm font-semibold text-muted uppercase tracking-wider">
          Consumers
        </div>

        <div className="animate_7000 opacity-0 absolute h-full left-0 border-r border-gray-400" />
        <div className="animate_7000 opacity-0 absolute w-4 border-t border-gray-400 top-0 left-0" />
        <div className="animate_7000 opacity-0 absolute w-4 border-b border-gray-400 bottom-0 left-0" />

        <div className="space-y-md">
          <div className="animate_8000 opacity-0 flex items-center justify-end">
            <div className={commonStyles.consumerList}>
              <div className="flex items-center">
                <FaMobileAlt className="fill-current w-4 h-full mr-1.5" />
                <div className="font-semibold">Apps</div>
              </div>
            </div>
          </div>

          <div className="animate_8500 opacity-0 flex items-center justify-end">
            <div className={commonStyles.consumerList}>
              <div className="flex items-center">
                <FaServer className="fill-current w-4 h-full mr-1.5" />
                <div className="font-semibold">Data Platforms</div>
              </div>
            </div>
          </div>

          <div className="animate_9000 opacity-0 flex items-center justify-end">
            <div className={commonStyles.consumerList}>
              <div className="flex items-center">
                <FaShareAlt className="fill-current w-4 h-full mr-1.5" />
                <div className="font-semibold">Other Services</div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
