import Select from 'react-select';
import { Button } from '../../../new-components/Button';
import { Switch } from '../../../new-components/Switch';
import hasuraIcon from '../images/hasura-icon-mono-light.svg';
import { consoleTypeDropDownArray } from './constants';
import { useMenuContentStyles } from './menu-container-styles';
import { ConsoleTypes, EnvStateArgs } from './types';

export type MenuPlacement = 'top' | 'bottom';

type MenuContentProps = {
  menuPlacement: MenuPlacement;
  minimized: boolean;
  handleTriggerClick: () => void;
  handleAdminSwitchChange: (enabled: boolean) => void;
  handleConsoleTypeChange: (
    option: { value: ConsoleTypes; label: string } | null
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
        <Button
          className="self-center"
          size="sm"
          onClick={() => handleMinimizeClick()}
        >
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
          <Select
            className="min-w-[100px]"
            menuPlacement="top"
            isSearchable={false}
            options={consoleTypeDropDownArray}
            value={consoleTypeDropDownArray.find(
              t => t.value === envArgsState.consoleType
            )}
            onChange={handleConsoleTypeChange}
          />
        </div>
      </div>
    </div>
  );
};
