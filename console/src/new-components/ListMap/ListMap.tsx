import React, { useState } from 'react';
import { FaCircle } from 'react-icons/fa';
import { RiCloseCircleFill } from 'react-icons/ri';
import { BsArrowRight } from 'react-icons/bs';
import { Select } from './Select';

interface TListMap {
  fromOptions: string[];
  toOptions: string[];
  name: string;
  fromLabel: string;
  toLabel: string;
  fromIcon?: React.ReactElement;
  toIcon?: React.ReactElement;
  maps: Record<string, string>;
  onChange: (e: Record<string, string>) => void;
}

const isValidMap = (map: { from: string; to: string }) =>
  !!(map.from && map.to);

export const ListMap = (props: TListMap) => {
  const {
    maps,
    fromLabel,
    toLabel,
    fromOptions,
    toOptions,
    onChange,
    name,
    fromIcon,
    toIcon,
  } = props;

  const [localMaps, setLocalMaps] = useState<{ from: string; to: string }[]>([
    ...Object.entries(maps).map(([from, to]) => ({
      from,
      to,
    })),
  ]);

  const [newMap, setNewMap] = useState<{ from: string; to: string }>({
    from: '',
    to: '',
  });

  const updateLocalMaps = (items: { from: string; to: string }[]) => {
    setLocalMaps(items);
    onChange(
      items.reduce(
        (resultMap, { from, to }) => ({ ...resultMap, [from]: to }),
        {}
      )
    );
  };

  const updateNewMap = (item: { from: string; to: string }) => {
    if (!isValidMap(item)) {
      setNewMap(item);
      return;
    }

    setNewMap({ from: '', to: '' });
    updateLocalMaps([...localMaps, item]);
    document.getElementById(`${name}_source_column`)?.focus();
  };

  return (
    <div
      id="reference"
      className="rounded bg-gray-50 border border-gray-300 p-md mb-md"
    >
      <div className="grid grid-cols-12 gap-3 mb-xs text-muted font-semibold">
        <div className="col-span-5 flex items-center">
          <FaCircle className="mr-1.5 text-green-600" />
          {fromIcon
            ? React.cloneElement(fromIcon, { className: 'mr-1.5' })
            : null}
          {fromLabel}
        </div>
        <div className="col-span-1 text-center" />
        <div className="col-span-5 flex items-center">
          <FaCircle className="mr-1.5 text-indigo-600" />
          {toIcon ? React.cloneElement(toIcon, { className: 'mr-1.5' }) : null}
          {toLabel}
        </div>
        <div className="col-span-1 text-center" />
      </div>
      {localMaps.map(({ from, to }, i) => {
        return (
          <div
            className="grid grid-cols-12 gap-3 mb-xs text-muted font-semibold"
            key={i}
          >
            <div className="col-span-5 flex items-center">
              <Select
                options={[
                  ...fromOptions.filter(
                    op => !localMaps.map(x => x.from).includes(op)
                  ),
                  from,
                ].filter(Boolean)}
                value={from}
                onChange={e => {
                  updateLocalMaps(
                    localMaps.map((item, j) => {
                      if (i !== j) return item;
                      return { ...item, from: e.target.value };
                    })
                  );
                }}
              />
            </div>
            <div className="col-span-1 flex items-center">
              <BsArrowRight />
            </div>
            <div className="col-span-5 flex items-center">
              <Select
                options={toOptions}
                value={to}
                onChange={e => {
                  updateLocalMaps(
                    localMaps.map((item, j) => {
                      if (i !== j) return item;
                      return { ...item, to: e.target.value };
                    })
                  );
                }}
              />
            </div>
            <div className="col-span-1 flex items-center">
              <RiCloseCircleFill
                onClick={() => {
                  updateLocalMaps([...localMaps.filter((_, j) => j !== i)]);
                }}
                className="cursor-pointer"
              />
            </div>
          </div>
        );
      })}
      <div className="grid grid-cols-12 gap-3 mb-xs text-muted font-semibold">
        <div className="col-span-5 flex items-center">
          <Select
            options={[
              ...fromOptions.filter(
                op => !localMaps.map(x => x.from).includes(op)
              ),
            ].filter(Boolean)}
            value={newMap.from}
            onChange={e => {
              updateNewMap({ ...newMap, from: e.target.value });
            }}
            id={`${name}_source_column`}
            data-testid={`${name}_from_user_input`}
            placeholder={`Select ${fromLabel.toLocaleLowerCase()}`}
          />
        </div>
        <div className="col-span-1 flex items-center">
          <BsArrowRight />
        </div>
        <div className="col-span-5 flex items-center">
          <Select
            options={toOptions}
            value={newMap.to}
            onChange={e => {
              updateNewMap({ ...newMap, to: e.target.value });
            }}
            data-testid={`${name}_to_user_input`}
            placeholder={`Select ${toLabel.toLocaleLowerCase()}`}
          />
        </div>
      </div>
    </div>
  );
};
