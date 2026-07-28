/// <reference types="vitest/config" />

// Configure Vitest (https://vitest.dev/config/)
import { defineConfig } from 'vitest/config'

export default defineConfig({
    test: {
        /* for example, use global to avoid globals imports (describe, test, expect): */
        // globals: true,
        dir: "./src"
    },
});