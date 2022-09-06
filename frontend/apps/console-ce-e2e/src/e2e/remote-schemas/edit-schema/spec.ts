import { getElementFromAlias } from '../../../helpers/eventHelpers';

type CustomizationSettingsType = {
  root_fields_namespace: string;
  type_names: {
    prefix: string;
    suffix: string;
    mapping: Record<string, string>;
  };
  field_names: {
    parent_type: string;
    prefix: string;
    suffix: string;
    mapping: Record<string, string>;
  }[];
};
export const modifyCustomization = (
  customizationSettings: CustomizationSettingsType | undefined
) => {
  cy.get(getElementFromAlias('remote-schema-customization-editor-expand-btn'))
    .should('exist')
    .click();

  // add root field name
  cy.get(getElementFromAlias('remote-schema-customization-root-field-input'))
    .clear()
    .type(customizationSettings?.root_fields_namespace || '');

  cy.get(
    getElementFromAlias('remote-schema-customization-type-name-prefix-input')
  )
    .clear()
    .type(customizationSettings?.type_names.prefix || '');

  cy.get(
    getElementFromAlias('remote-schema-customization-type-name-suffix-input')
  )
    .clear()
    .type(customizationSettings?.type_names.suffix || '');

  // add type name
  let key = Object.keys(customizationSettings?.type_names?.mapping || {})[0];
  cy.get(
    getElementFromAlias('remote-schema-customization-type-name-lhs-input')
  ).select(key);
  cy.get(
    getElementFromAlias('remote-schema-customization-type-name-0-rhs-input')
  )
    .clear()
    .type(customizationSettings?.type_names?.mapping[key] || '');

  cy.get(getElementFromAlias('remote-schema-editor')).should('exist').click();

  cy.get(getElementFromAlias('remote-schema-customization-open-field-mapping'))
    .should('exist')
    .click();

  // click the field mapping button
  cy.get(
    getElementFromAlias(
      'remote-schema-customization-field-type-parent-type-input'
    )
  ).select(customizationSettings?.field_names[0].parent_type || '');

  cy.get(
    getElementFromAlias(
      'remote-schema-customization-field-type-field-prefix-input'
    )
  )
    .clear()
    .type(customizationSettings?.field_names[0].prefix || '');

  cy.get(
    getElementFromAlias(
      'remote-schema-customization-field-type-field-suffix-input'
    )
  )
    .clear()
    .type(customizationSettings?.field_names[0].suffix || '');

  // remote-schema-customization-field-type-lhs-input
  key = Object.keys(customizationSettings?.field_names[0].mapping || {})[0];
  cy.get(
    getElementFromAlias('remote-schema-customization-field-type-lhs-input')
  ).select(key);
  // remote-schema-customization-field-type-rhs-input
  cy.get(
    getElementFromAlias('remote-schema-customization-field-type-0-rhs-input')
  )
    .clear()
    .type(customizationSettings?.field_names[0].mapping[key] || '');

  cy.get(getElementFromAlias('remote-schema-editor')).should('exist').click();

  cy.get(getElementFromAlias('add-field-customization'))
    .should('exist')
    .click();

  cy.get(getElementFromAlias('remote-schema-edit-save-btn'))
    .should('exist')
    .click();
};
