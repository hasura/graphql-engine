import type { ReactNode } from 'react';
import * as React from 'react';

import { HeroEnabled } from './Hero/HeroEnabled';
import { HeroDisabled } from './Hero/HeroDisabled';
import { HeroSkeleton } from './Hero/HeroSkeleton';
import { BadgeEnabled } from './Badge/BadgeEnabled';
import { BadgeDisabled } from './Badge/BadgeDisabled';
import { BadgeSkeleton } from './Badge/BadgeSkeleton';

type HeaderProps = {
  mode: 'enabled' | 'disabled' | 'skeleton';
};

export function Header(props: HeaderProps) {
  switch (props.mode) {
    case 'disabled':
      return <HeaderLayout badge={<BadgeDisabled />} hero={<HeroDisabled />} />;
    case 'skeleton':
      return <HeaderLayout badge={<BadgeSkeleton />} hero={<HeroSkeleton />} />;
    default:
      return <HeaderLayout badge={<BadgeEnabled />} hero={<HeroEnabled />} />;
  }
}

type HeaderLayoutProps = {
  badge: ReactNode;
  hero: ReactNode;
};

export function HeaderLayout(props: HeaderLayoutProps) {
  return (
    <>
      {/* The following markup has been stolen from the Prometheus page */}
      <div>
        <div className="flex items-center gap-4">
          <h1 className="text-xl font-semibold">
            OpenTelemetry <sup>(Beta)</sup>
          </h1>

          {props.badge}
        </div>
        <p className="text-muted">
          Export your OpenTelemetry traces from Hasura GraphQL Engine.{' '}
        </p>
      </div>
      {props.hero}
    </>
  );
}
