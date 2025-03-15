// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import remarkMath from 'remark-math';
import rehypeMathjax from 'rehype-mathjax';
import remarkDirective from 'remark-directive';
import remarkDirectiveRehype from 'remark-directive-rehype';
import rehypeComponents from 'rehype-components';
import {h} from 'hastscript';
import { directives } from './src/directives.ts';
import rehypeNumbered from './src/rehype-numbered.js';

// https://astro.build/config
export default defineConfig({
  site: 'https://yongqi.foo',
  base: 'it5100a-notes',
  markdown: {
    remarkPlugins: [ remarkMath, remarkDirective, remarkDirectiveRehype ],
    rehypePlugins: [ 
      [rehypeNumbered, { refName: 
        { eg: "Example", 
          def: "Definition", 
          thm: "Theorem",
          prop: "Proposition",
          conj: "Conjecture",
          lem: "Lemma",
          cor: "Corollary",
          noneg: "Nonexample"
        } }], 
      rehypeMathjax, 
      [rehypeComponents, {
        components: directives
      }] ]
  },
	integrations: [
		starlight({
      customCss: ['./src/global.css'],
			title: 'IT5100A Course Notes',
			social: {
				github: 'https://github.com/plilab',
			},
      components: {
        Sidebar: './src/components/starlight/Sidebar.astro',
        PageTitle: './src/components/starlight/PageTitle.astro',
        Hero: './src/components/starlight/Hero.astro'
      },
      expressiveCode: {
        styleOverrides: {
          borderRadius: '0.5rem'
        }
      },
			sidebar: [
        {
          label: 'Administrivia',
          autogenerate: { directory: 'admin' }
        },
        {
          label: 'Notes',
          items: [
            { 
              label: 'Course Introduction',
              autogenerate: { directory: 'notes/Course Introduction/' } 
            },
            { 
              label: 'Types',
              autogenerate: { directory: 'notes/Types/' } 
            },
            { 
              label: 'Typeclasses',
              autogenerate: { directory: 'notes/Typeclasses/' } 
            },
            { 
              label: 'Railway Pattern',
              autogenerate: { directory: 'notes/railway/' } 
            },
            { 
              label: 'Monads',
              autogenerate: { directory: 'notes/monads/' } 
            },
            { 
              label: 'Concurrent and Parallel Programming',
              autogenerate: { directory: 'notes/concurrent/' } 
            },
            {
              label: 'Recap',
              autogenerate: { directory: 'notes/recap/' }
            }
          ]
        },
        {
          label: 'Exercises',
          autogenerate: { directory: 'exercises' }
        },
        {
          label: 'Assignments',
          autogenerate: { directory: 'assignments' }
        }
			],
		}),
	],
});



