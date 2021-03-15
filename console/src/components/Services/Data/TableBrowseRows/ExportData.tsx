import React from 'react';
import { DropdownButton, MenuItem } from 'react-bootstrap';

export interface ExportDataProps {
  onExport: (type: 'CSV' | 'JSON') => Promise<Record<string, unknown>>;
}
const ExportData: React.FC<ExportDataProps> = ({ onExport }) => {
  const exportCSV = (
    e: React.MouseEvent<Record<string, unknown>, MouseEvent>
  ) => {
    e.preventDefault();
    onExport('CSV');
  };
  const exportJSON = (
    e: React.MouseEvent<Record<string, unknown>, MouseEvent>
  ) => {
    e.preventDefault();
    onExport('JSON');
  };

  return (
    <DropdownButton
      style={{ marginRight: 20 }}
      bsStyle="export"
      title="Export data"
      id="dropdown-basic"
    >
      <MenuItem eventKey="1" onClick={exportCSV}>
        Export Data CSV
      </MenuItem>
      <MenuItem eventKey="2" onClick={exportJSON}>
        Export Data JSON
      </MenuItem>
    </DropdownButton>
  );
};

export default ExportData;
