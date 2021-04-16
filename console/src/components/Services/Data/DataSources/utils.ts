export const getErrorMessageFromMissingFields = (
  host: string,
  port: string,
  username: string,
  database: string
) => {
  const missingFields = [];
  if (!host) {
    missingFields.push('host');
  }
  if (!port) {
    missingFields.push('port');
  }
  if (!username) {
    missingFields.push('username');
  }
  if (!database) {
    missingFields.push('database');
  }

  return `The following fields are required: ${missingFields
    .slice(0, missingFields.length - 1)
    .join(', ')} and ${missingFields[missingFields.length - 1]}`;
};

export const getDatasourceURL = (
  link: string | { from_env: string } | undefined
) => {
  if (!link) {
    return '';
  }
  if (typeof link === 'string') {
    return link.toString();
  }
  return link.from_env.toString();
};

export const readFile = (
  file: File | null,
  callback: (content: string) => void
) => {
  const reader = new FileReader();
  reader.onload = event => {
    const content = event.target!.result as string;
    callback(content);
  };

  reader.onerror = event => {
    console.error(`File could not be read! Code ${event.target!.error!.code}`);
  };

  if (file) reader.readAsText(file);
};
