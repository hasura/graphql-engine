import { Flex } from '@radix-ui/themes';
import globals from '../../../../../Globals';
import { NeonIcon } from './NeonIconSmall';
import { useShowNeonDashboardLink } from './useShowNeonDashboardLink';

type NeonDashboardLinkProps = {
  className?: string;
};

function NeonDashboardLinkContent() {
  const neonDashboardLink = `https://console.${globals.neonRootDomain}/app/projects`;

  return (
    <Flex align="center" justify="start">
      <Flex align="center">
        <div className="mr-2">
          <NeonIcon />
        </div>
        <div className="text-sm text-gray-700">
          <a
            className="hover:no-underline"
            href={neonDashboardLink}
            target="_blank"
            rel="noopener noreferrer"
          >
            Neon Console
          </a>
          <span className="ml-1">- Manage your Neon databases</span>
        </div>
      </Flex>
    </Flex>
  );
}

export function NeonDashboardLink(props: NeonDashboardLinkProps) {
  const { className } = props;
  const show = useShowNeonDashboardLink();

  if (!show) return null;

  return (
    <div className={className}>
      <NeonDashboardLinkContent />
    </div>
  );
}
