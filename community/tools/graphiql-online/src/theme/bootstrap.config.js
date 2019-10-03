/**
 * Bootstrap configuration for bootstrap-sass-loader
 *
 * Scripts are disabled to not load jQuery.
 * If you depend on Bootstrap scripts consider react-bootstrap instead.
 * https://github.com/react-bootstrap/react-bootstrap
 *
 * In order to keep the bundle size low in production
 * disable components you don't use.
 *
 */
module.exports = {
  preBootstrapCustomizations: './src/theme/variables.scss',
  mainSass: './src/theme/bootstrap.overrides.scss',
  verbose: false,
  debug: false,
  scripts: {
    transition: false,
    alert: false,
    button: true,
    carousel: false,
    collapse: false,
    dropdown: true,
    modal: true,
    tooltip: false,
    popover: false,
    scrollspy: false,
    tab: false,
    affix: false,
  },
  styles: {
    mixins: true,
    normalize: true,
    print: true,
    glyphicons: true,
    scaffolding: true,
    type: true,
    code: true,
    grid: true,
    tables: true,
    forms: true,
    buttons: true,
    'component-animations': true,
    dropdowns: true,
    'button-groups': true,
    'input-groups': true,
    navs: true,
    navbar: true,
    breadcrumbs: true,
    pagination: true,
    pager: true,
    labels: true,
    badges: true,
    jumbotron: true,
    thumbnails: true,
    alerts: true,
    'progress-bars': true,
    media: true,
    'list-group': true,
    panels: true,
    wells: true,
    'responsive-embed': true,
    close: true,
    modals: true,
    tooltip: true,
    popovers: true,
    carousel: false,
    utilities: true,
    'responsive-utilities': true,
  },
};
