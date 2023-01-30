import { trackGraphiQlCodeExporterClick } from '../../customAnalyticsEvents';

/**
 * Check if the clicked element is one of the Code Exporter elements we track and track it.
 */
export function onGraphiQlContainerClick(event: Event) {
  const clickedEl = event.target;
  const isDomElement = clickedEl instanceof Element;

  // This is a TS-only check
  if (!isDomElement) return;

  const isInsideCodeExporter = clickedEl.matches('div.doc-explorer-contents *');
  if (!isInsideCodeExporter) return;

  // -----------------------------------
  // JavaScript/TypeScript button
  if (
    clickedEl.matches('.toolbar-menu.toolbar-button[title=Language]') ||
    clickedEl.matches('.toolbar-menu.toolbar-button[title=Language] *')
  ) {
    trackGraphiQlCodeExporterClick('JavaScript/TypeScript');
  }

  // -----------------------------------
  // Fetch button
  if (
    clickedEl.matches('.toolbar-menu.toolbar-button[title=Mode]') ||
    clickedEl.matches('.toolbar-menu.toolbar-button[title=Mode] *')
  ) {
    trackGraphiQlCodeExporterClick('Fetch');
  }

  // -----------------------------------
  // server-side usage checkbox
  if (
    clickedEl.matches('input[id=server]') ||
    clickedEl.matches('input[id=server] *') ||
    clickedEl.matches('label[for=server]') ||
    clickedEl.matches('label[for=server] *')
  ) {
    trackGraphiQlCodeExporterClick('server-side usage');
  }

  // -----------------------------------
  // async/await checkbox
  if (
    clickedEl.matches('input[id=asyncAwait]') ||
    clickedEl.matches('input[id=asyncAwait] *') ||
    clickedEl.matches('label[for=asyncAwait]') ||
    clickedEl.matches('label[for=asyncAwait] *')
  ) {
    trackGraphiQlCodeExporterClick('async/await');
  }

  // -----------------------------------
  // copy button
  // The copy button is not identifiable at all because the only valid selector (.toolbar-button) is
  // shared amongst all the other button. It's structure is something like

  // <button class="toolbar-button"> <-- The button to click
  //   <div>Copied!</div> <-- The tooltip
  //   <svg> <-- The icon
  //     ...
  //   </svg>
  // </button>
  //
  // That's why we need to retrieve it from the "Copied!" tooltip.
  const copyButton = document.evaluate(
    "//*[contains(text(),'Copied!')]/parent::*",
    document,
    null,
    XPathResult.FIRST_ORDERED_NODE_TYPE,
    null
  ).singleNodeValue;

  if (copyButton === clickedEl || copyButton?.contains(clickedEl)) {
    trackGraphiQlCodeExporterClick('copy');
  }
}
