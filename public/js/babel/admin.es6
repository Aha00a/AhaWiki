function Main() {
    const useState = React.useState;
    const [v, setV] = useState(10);
    return (
        <div>
            <h1>{v}</h1>
            <button onClick={() => setV(v + 1)}>incr</button>
        </div>
    );
}

if (window.attachEvent) {
    window.attachEvent("onload", Page_Load);
    window.attachEvent("onunload", Page_Unload);
} else {
    window.addEventListener("DOMContentLoaded", Page_Load, false);
    window.addEventListener("unload", Page_Unload, false);
}

function Page_Load() {
    ReactDOM.render(<Main />, document.getElementById('main'));
}

function Page_Unload() {

}
