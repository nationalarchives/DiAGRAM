const { defineConfig } = require("cypress");

module.exports = defineConfig({
  e2e: {
    baseUrl: "https://dev-diagram.nationalarchives.gov.uk",
  },
});
