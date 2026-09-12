import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';

// public/js/AhaWiki.ReCaptcha.js: nothing at all when the page has no site key, a new token on
// every call, Google's script loaded once and only when first needed, and a failed load not
// remembered. The server's side, a save without a token refused, is WikiSaveReCaptchaSpec.
const source = fs.readFileSync('public/js/AhaWiki.ReCaptcha.js', 'utf8');

const boot = ({ siteKey = '', grecaptcha } = {}) => {
  const appended = [];
  const meta = siteKey ? { getAttribute: (name) => (name === 'content' ? siteKey : null) } : null;
  const windowObj = {};
  if (grecaptcha) windowObj.grecaptcha = grecaptcha;
  const documentObj = {
    querySelector: (selector) => (selector === 'meta[name="ahawiki-recaptcha-site-key"]' ? meta : null),
    createElement: (tag) => ({ tag }),
    head: { appendChild: (node) => { appended.push(node); } },
  };
  const sandbox = { window: windowObj, document: documentObj, setTimeout, clearTimeout };
  vm.createContext(sandbox);
  vm.runInContext(source, sandbox);
  return { reCaptcha: windowObj.AhaWiki.ReCaptcha, windowObj, appended };
};

const fakeGrecaptcha = () => {
  const calls = [];
  let issued = 0;
  return {
    calls,
    ready: (callback) => callback(),
    execute: (key, options) => {
      calls.push({ key, action: options.action });
      issued += 1;
      return Promise.resolve(`token-${issued}`);
    },
  };
};

test('without a site key there is no token to send and nothing is loaded', async () => {
  const { reCaptcha, appended } = boot();
  assert.equal(reCaptcha.isEnabled(), false);
  assert.equal(await reCaptcha.token('save'), '');
  await reCaptcha.preload();
  assert.equal(appended.length, 0);
});

test('every call asks Google again, so no token is sent twice', async () => {
  const g = fakeGrecaptcha();
  const { reCaptcha } = boot({ siteKey: 'site-key', grecaptcha: g });
  assert.equal(reCaptcha.isEnabled(), true);
  assert.equal(await reCaptcha.token('save'), 'token-1');
  assert.equal(await reCaptcha.token('kanban_save'), 'token-2');
  assert.deepEqual(g.calls, [
    { key: 'site-key', action: 'save' },
    { key: 'site-key', action: 'kanban_save' },
  ]);
});

test("Google's script is loaded once, on first use", async () => {
  const { reCaptcha, windowObj, appended } = boot({ siteKey: 'site key' });
  const first = reCaptcha.token('save');
  const second = reCaptcha.token('save');
  assert.equal(appended.length, 1);
  assert.equal(appended[0].src, 'https://www.google.com/recaptcha/api.js?render=site%20key');
  windowObj.grecaptcha = fakeGrecaptcha();
  appended[0].onload();
  assert.deepEqual([await first, await second], ['token-1', 'token-2']);
});

test('a failed load is not remembered, so the next save tries again', async () => {
  const { reCaptcha, windowObj, appended } = boot({ siteKey: 'site-key' });
  const failed = reCaptcha.token('save');
  appended[0].onerror();
  await assert.rejects(failed, /failed to load/);

  const retried = reCaptcha.token('save');
  assert.equal(appended.length, 2);
  windowObj.grecaptcha = fakeGrecaptcha();
  appended[1].onload();
  assert.equal(await retried, 'token-1');
});
