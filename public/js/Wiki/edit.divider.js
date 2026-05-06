$(function () {
    const divider = document.querySelector('div.ui > form > .divider');
    const left = document.querySelector('div.ui > form > div.left');

    let isDragging = false;
    divider.addEventListener("mousedown", (e) => {
        isDragging = true;
        document.body.style.userSelect = 'none';
    });

    document.addEventListener("mousemove", (e) => {
        if (!isDragging)
            return;

        console.log(e.x, left.getBoundingClientRect().left);

        const dividerWidthWithMargin = 25;
        const newWidth = e.clientX - left.getBoundingClientRect().left - dividerWidthWithMargin / 2;
        left.style.width = `${newWidth}px`;
    });

    document.addEventListener("mouseup", () => {
        isDragging = false;
        document.body.style.userSelect = '';
    });
});
