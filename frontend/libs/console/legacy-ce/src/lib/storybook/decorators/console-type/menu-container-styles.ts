import clsx from 'clsx';
import { MenuPlacement } from './MenuContent';

export const twStyles = {
  label: `font-bold text-[#2E3438] text-[12px]`,
  menuContainer: {
    base: `right-0 mr-4 rounded-t-md z-[99999] fixed pointer-events-auto`,
    position: {
      top: 'top-0',
      bottom: 'bottom-0',
    },
  },
  triggerButton: {
    base: `absolute right-0 -mr-1 transition-all`,
    position: {
      bottom: `bottom-0 mb-3`,
    },
    notMinimized: {
      base: `opacity-0 h-0 invisible`,
      translate: {
        top: `translate-x-1/2`,
        bottom: `-translate-x-1/2`,
      },
    },
  },
  mainMenuContainer: {
    base: `border border-b-0 bg-white flex items-start h-24 p-4 justify-end gap-5`,
    position: {
      top: `rounded-t-none rounded-b-md !border-b border-t-0`,
    },
    transition: `transition-all`,
    minimized: {
      base: `opacity-0 h-0 invisible`,
      translate: {
        top: `-translate-y-1/2`,
        bottom: `translate-y-1/2`,
      },
    },
  },
  controlContainer: `gap-1 flex flex-col`,
  button: `active:shadow-[0_0_1px_1px_#1B2738] active:opacity-70 h-[46px] mt-4 w-[46px] bg-[#1B2738] shadow-[0_0_2px_2px_#1B2738] rounded-full`,
};

// Functions to generate classNames
export const getMenuContainerClass = (menuPlacement: MenuPlacement): string =>
  clsx(
    twStyles.menuContainer.base,
    twStyles.menuContainer.position[menuPlacement]
  );

export const getTriggerButtonClass = (
  menuPlacement: MenuPlacement,
  minimized: boolean
): string =>
  clsx(
    twStyles.triggerButton.base,
    menuPlacement === 'bottom' && twStyles.triggerButton.position.bottom,
    !minimized && twStyles.triggerButton.notMinimized.base,
    !minimized && twStyles.triggerButton.notMinimized.translate[menuPlacement]
  );

export const getMainMenuContainerClass = (
  menuPlacement: MenuPlacement,
  minimized: boolean
): string =>
  clsx(
    twStyles.mainMenuContainer.base,
    menuPlacement === 'top' && twStyles.mainMenuContainer.position.top,
    twStyles.mainMenuContainer.transition,
    minimized && twStyles.mainMenuContainer.minimized.base,
    minimized && twStyles.mainMenuContainer.minimized.translate[menuPlacement]
  );

export function useMenuContentStyles({
  menuPlacement,
  minimized,
}: {
  menuPlacement: MenuPlacement;
  minimized: boolean;
}) {
  const menuContainerClass = getMenuContainerClass(menuPlacement);
  const triggerButtonClass = getTriggerButtonClass(menuPlacement, minimized);
  const mainMenuContainerClass = getMainMenuContainerClass(
    menuPlacement,
    minimized
  );

  return {
    styles: twStyles,
    menuContainerClass,
    triggerButtonClass,
    mainMenuContainerClass,
  };
}
