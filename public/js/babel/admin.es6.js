const h = React.createElement;

function useJson(url) {
  const [state, setState] = React.useState({
    loading: true,
    error: "",
    data: []
  });

  React.useEffect(function () {
    let isMounted = true;
    fetch(url, { credentials: "same-origin" })
      .then(function (res) {
        if (!res.ok) throw new Error("HTTP " + res.status);
        return res.json();
      })
      .then(function (data) {
        if (isMounted) setState({ loading: false, error: "", data: data });
      })
      .catch(function (err) {
        if (isMounted) setState({ loading: false, error: String(err), data: [] });
      });
    return function () {
      isMounted = false;
    };
  }, [url]);

  return state;
}

function Section(props) {
  return h(
    "section",
    { style: { marginBottom: "24px" } },
    h("h2", null, props.title),
    props.children
  );
}

function SitesTable(props) {
  if (props.loading) return h("p", null, "Loading sites...");
  if (props.error) return h("p", { style: { color: "red" } }, "Failed to load sites: " + props.error);
  return h(
    "table",
    { border: "1", cellPadding: "6", cellSpacing: "0", style: { borderCollapse: "collapse", width: "100%" } },
    h("thead", null, h("tr", null, h("th", null, "Seq"), h("th", null, "Name"))),
    h(
      "tbody",
      null,
      props.data.map(function (site) {
        return h("tr", { key: site.seq }, h("td", null, site.seq), h("td", null, site.name));
      })
    )
  );
}

function SiteUsersTable(props) {
  if (props.loading) return h("p", null, "Loading users...");
  if (props.error) return h("p", { style: { color: "red" } }, "Failed to load users: " + props.error);
  return h(
    "table",
    { border: "1", cellPadding: "6", cellSpacing: "0", style: { borderCollapse: "collapse", width: "100%" } },
    h("thead", null, h("tr", null, h("th", null, "User"), h("th", null, "Email"), h("th", null, "Nickname"), h("th", null, "Created"))),
    h(
      "tbody",
      null,
      props.data.map(function (user) {
        return h(
          "tr",
          { key: user.user + "-" + user.created },
          h("td", null, user.user),
          h("td", null, user.email),
          h("td", null, user.nickname),
          h("td", null, user.created)
        );
      })
    )
  );
}

function Main() {
  const sites = useJson("/api/admin/sites");
  const users = useJson("/api/admin/site-users");

  return h(
    "div",
    { style: { maxWidth: "1100px", margin: "16px auto", padding: "0 12px" } },
    h("h1", null, "Admin"),
    h("p", null, "Admin UI is now powered by React and Play API endpoints."),
    h(Section, { title: "Sites" }, h(SitesTable, sites)),
    h(Section, { title: "Site Users (current host)" }, h(SiteUsersTable, users))
  );
}

function Page_Load() {
  const rootElement = document.getElementById("main");
  if (!rootElement) return;
  if (ReactDOM.createRoot) {
    ReactDOM.createRoot(rootElement).render(h(Main));
    return;
  }
  ReactDOM.render(h(Main), rootElement);
}

if (window.attachEvent) {
  window.attachEvent("onload", Page_Load);
} else {
  window.addEventListener("DOMContentLoaded", Page_Load, false);
}
