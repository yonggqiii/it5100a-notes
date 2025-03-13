import { defineEcConfig } from 'astro-expressive-code';
import { pluginCollapsibleSections } from '@expressive-code/plugin-collapsible-sections';
import { pluginLineNumbers } from '@expressive-code/plugin-line-numbers';
export default defineEcConfig({
  plugins: [pluginCollapsibleSections(), pluginLineNumbers()],
  // You can set configuration options here
  themes: ['dracula', 'catppuccin-latte'],
  styleOverrides: {
    // You can also override styles
    borderRadius: '0.5rem',
    frames: {
      shadowColor: 'var(--sl-color-blue-low)',
    },
  },
})
