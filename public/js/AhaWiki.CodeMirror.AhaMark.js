(() => {
    function registerAhaMarkMode() {
        if (typeof window.CodeMirror === 'undefined')
            return 'ahamark';

        if (window.CodeMirror.modes && window.CodeMirror.modes.ahamark)
            return 'ahamark';

        CodeMirror.defineMode('ahamark', function () {
            return {
                token: function (stream, state) {
                    if (stream.sol()) {
                        state.inTripleInterpreter = false;
                        if (stream.match(/\s*={1,6}>?\s.+/))
                            return 'ahamark-header';
                        if (stream.match(/\s*\[\[\[#![^\]]*/)) {
                            state.inTripleInterpreter = true;
                            return 'ahamark-interpreter';
                        }
                    }

                    if (state.inTripleInterpreter && stream.match(/.*\]\]\]/)) {
                        state.inTripleInterpreter = false;
                        return 'ahamark-interpreter';
                    }
                    if (state.inTripleInterpreter) {
                        stream.skipToEnd();
                        return 'ahamark-interpreter';
                    }

                    if (stream.match(/\[(?!\[)[^\]\n]+\]/))
                        return 'ahamark-link';
                    if (stream.match(/\[\[[^\]]+\]\]/))
                        return 'ahamark-macro';
                    if (stream.match(/\[\d{4}-\d{2}-\d{2}(?:\s+\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2})?\]/))
                        return 'ahamark-date';
                    if (stream.match(/\*\*[^*]+\*\*/))
                        return 'ahamark-bold';
                    if (stream.match(/''[^']+''/))
                        return 'ahamark-italic';
                    if (stream.match(/\{\{[^}]+\}\}/))
                        return 'ahamark-tag';

                    stream.next();
                    return null;
                },
                startState: function () {
                    return { inTripleInterpreter: false };
                }
            };
        });

        return 'ahamark';
    }

    window.AhaWikiCodeMirrorAhaMarkMode = registerAhaMarkMode;
})();
