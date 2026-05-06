$(function () {
    $('#generateSignedViewUrl').on('click', async function (event) {
        event.preventDefault();
        const $link = $(this);
        try {
            const params = new URLSearchParams({
                name: String($link.data('page-name') || ''),
                revision: String($link.data('revision') || ''),
                action: 'view',
            });
            const response = await fetch('/api/Admin/SignedReadUrl?' + params.toString());
            const payload = await response.json();
            if (!response.ok) {
                alert(payload?.error || 'Failed to generate signed URL.');
                return;
            }

            const signedUrl = payload?.signedUrl || '';
            if (!signedUrl) {
                alert('Signed URL is empty.');
                return;
            }

            if (navigator.clipboard && navigator.clipboard.writeText) {
                await navigator.clipboard.writeText(signedUrl);
                alert('Signed URL copied to clipboard:\n' + signedUrl);
            } else {
                window.prompt('Signed URL', signedUrl);
            }
        } catch (error) {
            console.error(error);
            alert('Failed to generate signed URL.');
        }
    });
});
