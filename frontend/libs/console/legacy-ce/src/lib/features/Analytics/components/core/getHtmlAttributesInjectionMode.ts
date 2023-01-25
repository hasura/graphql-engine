export function getHtmlAttributesInjectionMode(
  childrenType: 'text' | 'htmlElement' | 'reactComponent',
  passHtmlAttributesToChildren = false
) {
  switch (childrenType) {
    case 'htmlElement':
      return 'passHtmlAttributesToChildren';

    case 'reactComponent':
      return passHtmlAttributesToChildren
        ? 'passHtmlAttributesToChildren'
        : 'wrapInDiv';

    default:
      return 'wrapInDiv';
  }
}
