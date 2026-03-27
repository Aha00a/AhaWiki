# Front-end admin build

`app/assets/js/admin.jsx` is bundled to `public/js/babel/admin.js`.

## One-time build

```bash
npm install
npm run build:admin
```

## Auto rebuild (watch mode)

```bash
npm run watch:admin
```

This is a Node.js + esbuild flow, so Bun/TypeScript are not required.
The old Bun script (`scripts/build-admin.ts`) is still kept for compatibility.
