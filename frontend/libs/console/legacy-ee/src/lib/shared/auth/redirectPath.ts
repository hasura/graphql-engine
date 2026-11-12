// Return a console-local path. BrowserRouter adds urlPrefix exactly once.
// Also tolerate old bookmarks/SSO state that already include the basename.
export const getLoginRedirectPath = (
  redirectUrl: string | null | undefined,
  urlPrefix: string,
) => {
  if (!redirectUrl?.startsWith('/') || redirectUrl.startsWith('//')) {
    return '/';
  }

  const prefix = urlPrefix.replace(/\/$/, '');
  let path = redirectUrl;
  if (
    prefix &&
    (path === prefix ||
      path.startsWith(`${prefix}/`) ||
      path.startsWith(`${prefix}?`) ||
      path.startsWith(`${prefix}#`))
  ) {
    path = path.slice(prefix.length);
  }

  // Reject URL-like paths and auth-page targets to avoid redirect loops.
  if (
    path.startsWith('//') ||
    path.includes('\\') ||
    Array.from(path).some((char) => {
      const code = char.charCodeAt(0);
      return code < 32 || code === 127;
    })
  ) {
    return '/';
  }

  if (!path || path.startsWith('?') || path.startsWith('#')) {
    path = `/${path}`;
  }

  if (/^\/(?:login|oauth2\/callback)\/?(?:[?#]|$)/.test(path)) {
    return '/';
  }

  return path;
};
