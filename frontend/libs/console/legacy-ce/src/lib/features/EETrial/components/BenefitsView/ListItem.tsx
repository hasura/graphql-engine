import React, { ReactElement } from 'react';
import { Analytics } from '../../../Analytics';

type Props = {
  label: string;
  icon: string | ReactElement;
  url: string;
  id: string;
};

export function ListItem(props: Props) {
  const { label, icon, url, id } = props;
  return (
    <Analytics name={`ee-benefits-${id}-link`}>
      <div className="px-xs border-b flex items-center">
        {typeof icon === 'string' ? (
          <img className="pb-xs h-7" src={icon} alt={label} />
        ) : (
          <div className="pb-xs text-muted">{icon}</div>
        )}
        <a href={url} target="_blank" rel="noopener noreferrer">
          <p className="pb-xs ml-1 text-secondary">{label}</p>
        </a>
      </div>
    </Analytics>
  );
}
