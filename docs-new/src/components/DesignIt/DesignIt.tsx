import { cssVars } from '@site/src/constants';
import { getVar, setVar } from '@site/src/utils';
import React, { useEffect, useState } from 'react';


export default function DesignIt(): JSX.Element {
  const [activeVarGroup, setActiveVarGroup] = useState("colors");
  const [activeVar, setActiveVar] = useState("--ifm-color-primary");
  const [activeVarValue, setActiveVarValue] = useState(cssVars[activeVarGroup][activeVar]);
  const [customVar, setCustomVar] = useState("");
  const [customVarValue, setCustomVarValue] = useState("");

  useEffect(() => setActiveVarValue(getVar(activeVar)?.trim()), [activeVar]);

  return (
      <div>
        <select onChange={({target}) => setActiveVarGroup(target.value)}>
          {Object.entries(cssVars).map(([key, value]) => (
            <option value={key}>{key}</option>
          ))}
        </select>
        <select onChange={({target}) => setActiveVar(target.value)}>
          {Object.entries(cssVars[activeVarGroup]).map(([key, value]) => (
            <option value={key}>{key}</option>
          ))}
        </select>
        {activeVarGroup === "colors" && <input type="color" value={activeVarValue} />}
        <input name={activeVar} onChange={({target}) => setActiveVarValue(target.value?.trim())} value={activeVarValue} />
        <button onClick={() => setVar(activeVar, activeVarValue)}>apply</button>
        <div>
          <p>Try custom one</p>
          <input name="custom-key" onChange={({target}) => setCustomVar(target.value?.trim())} value={customVar} />
          <input name="custom-var" onChange={({target}) => setCustomVarValue(target.value?.trim())} value={customVarValue} />
        </div>
        <button onClick={() => setVar(customVar, customVarValue)}>custom apply</button>
      </div>
  );
}
