module.exports = {
  presets: [
    '@vue/app'
  ],
  plugins: [
    ["transform-imports", {
      "vuetify": {
        "transform": "vuetify/es5/components/${member}",
        "preventFullImport": true
      }
    }]
  ]
}
