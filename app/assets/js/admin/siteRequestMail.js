// The letter the owner sends once a request for a wiki has been answered.
//
// The app sends nothing itself: it has no way to send mail. Until 2026-11-14 the only way a
// requester learned the answer was to open the request page again. The admin page now offers a
// mailto link, which opens the owner's own mail program with this letter written and the
// requester's address filled in. The owner reads it, changes what they like, and sends it.
//
// Two things keep it short. A mail program is handed the link as one URL, some take no more than
// about two thousand characters, and what does not fit is cut off without a word -- while Hangul
// costs nine characters each once encoded. And nothing the requester typed is repeated: a site
// name may be 200 characters and a reason for rejecting 1000. The reason is on the request page,
// so a rejection points there.
//
// Both languages, because the page a requester arrives from is Korean and the form they fill in
// is English.

function approvedLetter(row) {
    return {
        subject: `[AhaWiki] 위키가 열렸습니다 — ${row.domain}`,
        lines: [
            "안녕하세요. 신청하신 위키가 열렸습니다.",
            `https://${row.domain}/`,
            "",
            "위 주소에서 Login 을 누르고, 신청할 때 쓰신 Google 계정으로 로그인하세요. 그 계정이 위키의 관리자이고, 첫 화면에 시작하는 방법이 있습니다.",
            "",
            "Your wiki is open at the address above. Log in there with the Google account you asked with: it is the wiki's administrator, and the first page says how to begin.",
        ],
    };
}

function rejectedLetter(row) {
    // The request page is on the host the wikis hang under: the address minus its first label.
    const parentDomain = row.domain.split(".").slice(1).join(".");
    return {
        subject: `[AhaWiki] 위키 신청에 대한 답 — ${row.domain}`,
        lines: [
            `안녕하세요. 신청하신 위키(${row.domain})는 열어 드리지 못했습니다. 사유는 신청 화면에 있습니다.`,
            `https://${parentDomain}/site/request`,
            "",
            `Your request for ${row.domain} was not approved. The reason is on the request page above.`,
        ],
    };
}

const LETTERS = {Approved: approvedLetter, Rejected: rejectedLetter};

/** `{to, subject, body}` for a request that has an answer and a requester to tell, else null. */
export function siteRequestMail(row) {
    const letter = row && row.email && row.domain ? LETTERS[row.status] : null;
    if (!letter) return null;
    const {subject, lines} = letter(row);
    // A mailto link spells a line break as CR LF.
    return {to: row.email, subject, body: lines.join("\r\n")};
}

/** The same letter as a `mailto:` link, or null. */
export function siteRequestMailto(row) {
    const mail = siteRequestMail(row);
    if (!mail) return null;
    // The address keeps its @; a mail program need not decode one in the recipient.
    const to = encodeURIComponent(mail.to).replace(/%40/g, "@");
    return `mailto:${to}?subject=${encodeURIComponent(mail.subject)}&body=${encodeURIComponent(mail.body)}`;
}
