import React from 'react';
import { Flex, Grid } from '@radix-ui/themes';
import { Button, Input } from '@hasura/shared/ui';
import { addPlaceholderValue } from '../utils';
import { NameValue } from '@hasura/shared/types';

interface KeyValueInputProps {
  pairs: NameValue[];
  setPairs: (h: NameValue[]) => void;
  testId?: string;
}

// Stable React key per row, keyed by the NameValue OBJECT's identity. The pair
// objects keep their identity across edits (typing mutates the object in place;
// removal slices the array but reuses the element objects), so this gives a key
// that survives typing (no remount / no focus loss) and row removal — without
// serializing an id into the NameValue data or using an array index / random
// value. Module-scoped (not a hook ref) and keyed by a WeakMap so entries are
// garbage-collected with their pair objects.
const rowKeyMap = new WeakMap<NameValue, string>();
let rowKeyCounter = 0;
const keyForPair = (pair: NameValue): string => {
  const existing = rowKeyMap.get(pair);
  if (existing) return existing;
  rowKeyCounter += 1;
  const key = `kv-row-${rowKeyCounter}`;
  rowKeyMap.set(pair, key);
  return key;
};

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
          <Grid
            key={keyForPair(pair)}
            className="mb-2"
            gap="3"
            columns={{
              initial: '1',
              sm: '3',
            }}
          >
            <Input
              value={name}
              onChange={setPairKey}
              type="text"
              placeholder="Key..."
              data-test={`transform-${testId}-kv-key-${i.toString()}`}
              full
            />
            <Input
              value={value}
              onChange={setPairValue}
              type="text"
              placeholder="Value..."
              data-test={`transform-${testId}-kv-value-${i.toString()}`}
              full
            />
            {i < pairs.length - 1 ? (
              <Flex align="end">
                <Button
                  type="button"
                  size="sm"
                  mode="destructive"
                  onClick={removePair}
                  data-test={`transform-${testId}-kv-remove-button-${i.toString()}`}
                >
                  Remove
                </Button>
              </Flex>
            ) : (
              <Flex align="end" />
            )}
          </Grid>
        );
      })}
    </>
  );
};

export default KeyValueInput;
