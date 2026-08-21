# Front-end admin build

`app/assets/js/admin.jsx` is bundled to `public/js/babel/admin.js`.

## One-time build

```bash
npm install
npm run admin:build
```

## Auto rebuild (watch mode)

```bash
npm run admin:watch
```

This is a Node.js + esbuild flow (`scripts/admin.mjs`); Bun and TypeScript are not required.
