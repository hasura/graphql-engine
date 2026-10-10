import { DropdownMenu, Flex } from '@radix-ui/themes';
import { useAppearance } from './ThemeProvider';
import { BsMoon, BsSun } from 'react-icons/bs';

/**
 * A dark-mode toggle as an ordinary Radix DropdownMenu action item (role
 * `menuitem`). Its label is contextual — it reads "Dark mode" while in light
 * appearance and "Light mode" while in dark appearance (with a matching sun/moon
 * icon) — and selecting it toggles the appearance and closes the menu, like any
 * other action item. Operable by mouse and keyboard (Enter/Space). Designed to be
 * dropped into either the CE `UserMenu` (shared DropdownMenu `items` array) or the
 * EE header dropdown (`@radix-ui/themes` DropdownMenu.Content) — both render it
 * inside a DropdownMenu.Content. Must be used within an AppTheme/ThemeProvider.
 */
export function ThemeToggleMenuItem() {
  const { appearance, toggleAppearance } = useAppearance();

  return (
    <DropdownMenu.Item
      onSelect={() => {
        toggleAppearance();
      }}
    >
      <Flex align="center" gap="2">
        {appearance === 'dark' ? (
          <BsSun aria-hidden="true" className="w-5 h-5" />
        ) : (
          <BsMoon aria-hidden="true" />
        )}
        {appearance === 'dark' ? 'Light mode' : 'Dark mode'}
      </Flex>
    </DropdownMenu.Item>
  );
}
