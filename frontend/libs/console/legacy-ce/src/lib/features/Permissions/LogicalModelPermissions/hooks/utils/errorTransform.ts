export function errorTransform(error: any) {
  const err = error as Record<string, any>;

  let message = '';
  let name = `Error code: ${err.code}`;

  if ('internal' in err) {
    name = err?.internal?.[0]?.name ?? name;
    message = err?.internal?.[0]?.reason ?? 'Internal error';
  } else message = err.error;

  return {
    name,
    message,
  };
}
