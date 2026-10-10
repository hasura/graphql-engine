import { ReactNode } from 'react';
import { Theme } from '@radix-ui/themes';
import { ToastsHub } from '../components';
import { ThemeProvider, useAppearance } from './ThemeProvider';

import 'react-datepicker/dist/react-datepicker.css';
import './theme.css';
import './date-input.css';

const AppThemeBody = ({ children }: { children?: ReactNode }) => {
  const { appearance } = useAppearance();

  return (
    <Theme id="hasura-theme" accentColor="indigo" appearance={appearance}>
      {children}
      {/* ToastsHub (react-hot-toast) is position:fixed, so rendering it INSIDE
          the single root Theme keeps its content themed (appearance + Radix
          tokens) without introducing a SECOND root Theme. A sibling <Theme>
          would be `data-is-root-theme` and get `min-height: 100vh` + its own
          background, painting an extra blank viewport-height block below the
          app. */}
      <ToastsHub />
    </Theme>
  );
};

export const AppTheme = ({ children }: { children?: ReactNode }) => {
  return (
    <ThemeProvider>
      <AppThemeBody>{children}</AppThemeBody>
    </ThemeProvider>
  );
};
