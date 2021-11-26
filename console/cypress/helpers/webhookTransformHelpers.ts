import { getElementFromAlias } from './dataHelpers';

export const togglePayloadTransformSection = () => {
  cy.getBySel('toggle-payload-transform').click({
    force: true,
  });
};

export const toggleRequestTransformSection = () => {
  cy.getBySel('toggle-request-transform').click({
    force: true,
  });
};

export const clearRequestUrl = () => {
  cy.get(
    getElementFromAlias('transform-requestUrl')
  ).type('{selectall}{backspace}', { force: true });
};

export const typeIntoRequestUrl = (content: string) => {
  cy.getBySel('transform-requestUrl').type(content, {
    parseSpecialCharSequences: false,
  });
};

export const checkTransformRequestUrlError = (
  exists: boolean,
  error?: string
) => {
  if (exists) {
    if (error) {
      cy.getBySel('transform-requestUrl-error')
        .should('exist')
        .and('contain', error);
    } else {
      cy.getBySel('transform-requestUrl-error').should('exist');
    }
  } else {
    cy.getBySel('transform-requestUrl-error').should('not.exist');
  }
};

export const typeIntoRequestQueryParams = (
  queryParams: { key: string; value: string }[]
) => {
  queryParams.forEach((q, i) => {
    cy.getBySel(`transform-kv-key-${i}`).type(q.key, {
      parseSpecialCharSequences: false,
    });
    cy.getBySel(`transform-kv-value-${i}`).type(q.value, {
      parseSpecialCharSequences: false,
    });
  });
};

export const checkTransformRequestUrlPreview = (previewText: string) => {
  cy.getBySel('transform-requestUrl-preview').should('have.value', previewText);
};

export const clearPayloadTransformBody = (textArea: number) => {
  cy.get('textarea').eq(textArea).type('{selectall}', { force: true });
  cy.get('textarea').eq(textArea).trigger('keydown', {
    keyCode: 46,
    which: 46,
    force: true,
  });
};

export const typeIntoTransformBody = (content: string, textArea: number) => {
  cy.get('textarea')
    .eq(textArea)
    .type(content, { force: true, parseSpecialCharSequences: false });
};

export const checkTransformRequestBodyError = (exists: boolean) => {
  if (exists) {
    cy.getBySel('transform-requestBody-error').should('exist');
  } else {
    cy.getBySel('transform-requestBody-error').should('not.exist');
  }
};
