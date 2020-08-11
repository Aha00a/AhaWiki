function Main() {
  const useState = React.useState;
  const [v, setV] = useState(10);
  return /*#__PURE__*/React.createElement("div", null, /*#__PURE__*/React.createElement("h1", null, v), /*#__PURE__*/React.createElement("button", {
    onClick: () => setV(v + 1)
  }, "incr"));
}

if (window.attachEvent) {
  window.attachEvent("onload", Page_Load);
  window.attachEvent("onunload", Page_Unload);
} else {
  window.addEventListener("DOMContentLoaded", Page_Load, false);
  window.addEventListener("unload", Page_Unload, false);
}

function Page_Load() {
  ReactDOM.render( /*#__PURE__*/React.createElement(Main, null), document.getElementById('main'));
}

function Page_Unload() {}

//# sourceMappingURL=admin.es6.js.map