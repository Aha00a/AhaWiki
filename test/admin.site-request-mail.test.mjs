// The app sends nothing to someone whose request for a wiki has been answered: it has no way to
// send mail. The admin page instead opens the owner's own mail program with the letter written
// (app/assets/js/admin/siteRequestMail.js). This is what that letter and its link must hold.
import test from 'node:test';
import assert from 'node:assert/strict';
import { siteRequestMail, siteRequestMailto } from '../app/assets/js/admin/siteRequestMail.js';

const approved = {
    seq: 7, user: 10, userNickname: 'alicewiki', email: 'alice@example.com', name: 'My wiki',
    domain: 'alices.wiki.test', isPublic: true, purpose: 'notes', status: 'Approved', rejectReason: null,
};
const rejected = { ...approved, domain: 'bobs.wiki.test', status: 'Rejected', rejectReason: 'no' };

const parse = (mailto) => {
    const [, to, query] = mailto.match(/^mailto:([^?]+)\?(.*)$/);
    const params = new URLSearchParams(query.replace(/\+/g, '%2B'));
    return { to, subject: params.get('subject'), body: params.get('body') };
};

test('an approved request gets a letter with the address of the wiki and what to do there', () => {
    const { to, subject, body } = parse(siteRequestMailto(approved));
    assert.equal(to, 'alice@example.com');
    assert.match(subject, /alices\.wiki\.test/);
    assert.match(body, /https:\/\/alices\.wiki\.test\//);
    // Said in both languages: the page the requester came from is Korean, the form is English.
    assert.match(body, /Login/);
    assert.match(body, /Google 계정으로 로그인/);
    assert.match(body, /Log in there with the Google account/);
});

test('a rejected request is sent to the page that holds the reason, not the reason itself', () => {
    const { to, body } = parse(siteRequestMailto({ ...rejected, rejectReason: 'x'.repeat(1000) }));
    assert.equal(to, 'alice@example.com');
    assert.match(body, /bobs\.wiki\.test/);
    assert.match(body, /https:\/\/wiki\.test\/site\/request/);
    assert.doesNotMatch(body, /xxxx/);
});

test('there is no letter while there is no answer, or nobody to write to', () => {
    assert.equal(siteRequestMailto({ ...approved, status: 'Pending' }), null);
    assert.equal(siteRequestMailto({ ...approved, status: 'Canceled' }), null);
    assert.equal(siteRequestMailto({ ...approved, email: null }), null);
    assert.equal(siteRequestMail(null), null);
});

// A mail program is handed this as one URL, and some take no more than about two thousand
// characters of it; what does not fit is cut off without a word. Hangul is nine characters each
// once encoded, so the letters are kept short and say nothing a requester typed -- a site name may
// be 200 characters and a reason 1000.
test('the link fits where long links are cut', () => {
    const longest = 'a'.repeat(63) + '.wiki.test';
    for (const row of [{ ...approved, domain: longest, name: '가'.repeat(200) }, { ...rejected, domain: longest, rejectReason: '가'.repeat(1000) }]) {
        const mailto = siteRequestMailto(row);
        assert.ok(mailto.length < 2000, `${row.status}: ${mailto.length} characters`);
    }
});

test('lines break the way a mailto link has to spell them, and the address keeps its @', () => {
    const mailto = siteRequestMailto({ ...approved, email: 'a+b@example.com' });
    assert.match(mailto, /^mailto:a%2Bb@example\.com\?subject=/);
    assert.match(mailto, /%0D%0A/);
    assert.doesNotMatch(mailto, /[^D]%0A/);
});
