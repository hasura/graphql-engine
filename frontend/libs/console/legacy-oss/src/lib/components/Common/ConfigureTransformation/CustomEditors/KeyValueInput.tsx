import React from 'react';
import {
  buttonShadow,
  inputStyles,
  focusYellowRing,
  addPlaceholderValue,
} from '../utils';
import { KeyValuePair } from '../stateDefaults';

interface KeyValueInputProps {
  pairs: KeyValuePair[];
  setPairs: (h: KeyValuePair[]) => void;
  testId?: string;
}

const KeyValueInput: React.FC<KeyValueInputProps> = ({
  pairs,
  setPairs,
  testId,
}) => {
  return (
    <>
      {pairs.map((pair, i) => {
        const setPairKey = (e: React.ChangeEvent<HTMLInputElement>) => {
          const newPairs = [...pairs];
          newPairs[i].name = e.target.value;
          addPlaceholderValue(newPairs);
          setPairs(newPairs);
        };

        const setPairValue = (e: React.ChangeEvent<HTMLInputElement>) => {
          const newPairs = [...pairs];
          newPairs[i].value = e.target.value;
          addPlaceholderValue(newPairs);
          setPairs(newPairs);
        };

        const removePair = () => {
          const newPairs = [...pairs];
          setPairs([...newPairs.slice(0, i), ...newPairs.slice(i + 1)]);
        };

        const { name, value } = pair;
        return (
          <React.Fragment key={`pair-${i.toString()}`}>
            <div>
              <input
                value={name}
                onChange={setPairKey}
                type="text"
                name="table_name"
                id="table_name"
                className={`w-full ${inputStyles}`}
                placeholder="Key..."
                data-test={`transform-${testId}-kv-key-${i.toString()}`}
              />
            </div>
            <div>
              <input
                value={value}
                onChange={setPairValue}
                type="text"
                name="table_name"
                id="table_name"
                className={`w-full ${inputStyles}`}
                placeholder="Value..."
                data-test={`transform-${testId}-kv-value-${i.toString()}`}
              />
            </div>
            {i < pairs.length - 1 ? (
              <div className="flex items-end">
                <button
                  type="button"
                  onClick={removePair}
                  data-test={`transform-${testId}-kv-remove-button-${i.toString()}`}
                  className={`flex items-center text-sm font-medium h-btn px-3 ${buttonShadow} ${focusYellowRing}`}
                >
                  Remove
                </button>
              </div>
            ) : (
              <div className="flex items-end" />
            )}
          </React.Fragment>
        );
      })}
    </>
  );
};

export default KeyValueInput;
