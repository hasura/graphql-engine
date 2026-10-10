import hasuraIcon from '../images/hasura-icon-mono-light.svg';
import { consoleTypeDropDownArray } from './constants';
import { useMenuContentStyles } from './menu-container-styles';
import { ConsoleTypes, EnvStateArgs } from './types';
import { Button, Switch } from '@radix-ui/themes';

export type MenuPlacement = 'top' | 'bottom';

type MenuContentProps = {
  menuPlacement: MenuPlacement;
  minimized: boolean;
  handleTriggerClick: () => void;
  handleAdminSwitchChange: (enabled: boolean) => void;
  handleConsoleTypeChange: (
    option: { value: ConsoleTypes; label: string } | null,
  ) => void;
  handleMinimizeClick: () => void;
  envArgsState: EnvStateArgs;
};
export const MenuContent = ({
  menuPlacement,
  minimized,
  handleMinimizeClick,
  handleTriggerClick,
  handleAdminSwitchChange,
  handleConsoleTypeChange,
  envArgsState,
}: MenuContentProps) => {
  const {
    styles,
    mainMenuContainerClass,
    triggerButtonClass,
    menuContainerClass,
  } = useMenuContentStyles({ menuPlacement, minimized });

  return (
    <div data-chromatic="ignore" className={menuContainerClass}>
      <div className={triggerButtonClass}>
        <button
          type="button"
          onClick={handleTriggerClick}
          className={styles.button}
        >
          <img
            src={hasuraIcon}
            style={{ height: 26 }}
            alt="Console Type Dev Tools"
          />
        </button>
      </div>
      <div className={mainMenuContainerClass}>
        <Button className="self-center" onClick={() => handleMinimizeClick()}>
          Close
        </Button>
        <div className={styles.controlContainer}>
          <div className={styles.label}>Admin Secret:</div>
          <Switch
            checked={envArgsState.adminSecret}
            className="mt-2"
            onCheckedChange={handleAdminSwitchChange}
          />
        </div>
        <div className={styles.controlContainer}>
          <div className={styles.label}>Console Type:</div>
          <select
            className="min-w-[100px]"
            value={envArgsState.consoleType}
            onChange={(ev) => {
              const result = consoleTypeDropDownArray.find(
                (consoleType) => consoleType.value === ev.target.value,
              );

              if (result) {
                handleConsoleTypeChange(result);
              }
            }}
          >
            {consoleTypeDropDownArray.map((consoleType) => (
              <option key={consoleType.value} value={consoleType.value}>
                {consoleType.label}
              </option>
            ))}
          </select>
        </div>
      </div>
    </div>
  );
};
