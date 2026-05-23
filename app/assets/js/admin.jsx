import React from "react";
import {createRoot} from "react-dom/client";
import {createBrowserRouter, RouterProvider} from "react-router-dom";
import {MantineProvider} from "@mantine/core";
import AdminLayout from "./admin/layout/AdminLayout.jsx";
import SiteLayout from "./admin/layout/SiteLayout.jsx";
import DashboardPage from "./admin/pages/DashboardPage.jsx";
import SitesPage from "./admin/pages/SitesPage.jsx";
import AllUsersPage from "./admin/pages/AllUsersPage.jsx";
import UserViewsPage from "./admin/pages/UserViewsPage.jsx";
import SiteDetailPage from "./admin/pages/SiteDetailPage.jsx";
import SiteConfigPage from "./admin/pages/SiteConfigPage.jsx";
import SiteCachePage from "./admin/pages/SiteCachePage.jsx";
import SitePermissionPage from "./admin/pages/SitePermissionPage.jsx";
import SiteAdminsPage from "./admin/pages/SiteAdminsPage.jsx";
import RecentChangesPage from "./admin/pages/RecentChangesPage.jsx";
import AccessLogsPage from "./admin/pages/AccessLogsPage.jsx";
import S3BrowserPage from "./admin/pages/S3BrowserPage.jsx";
import CrawlerCachePage from "./admin/pages/CrawlerCachePage.jsx";

const router = createBrowserRouter([
    {
        path: "/Admin",
        element: <AdminLayout/>,
        children: [
            {index: true, element: <DashboardPage/>},
            {path: "Site", element: <SitesPage/>},
            {path: "Sites", element: <SitesPage/>},
            {
                path: "Site/:siteSeq",
                element: <SiteLayout/>,
                children: [
                    {index: true, element: <SiteDetailPage/>},
                    {path: "Config", element: <SiteConfigPage/>},
                    {path: "Cache", element: <SiteCachePage/>},
                    {path: "Permission", element: <SitePermissionPage/>},
                    {path: "Admins", element: <SiteAdminsPage/>},
                ],
            },
            {path: "User", element: <AllUsersPage/>},
            {path: "AllUsers", element: <AllUsersPage/>},
            {path: "User/UserViewHistory", element: <UserViewsPage/>},
            {path: "UserViews", element: <UserViewsPage/>},
            {path: "AccessLog", element: <AccessLogsPage/>},
            {path: "AccessLogs", element: <AccessLogsPage/>},
            {path: ":siteSeq/AccessLog", element: <AccessLogsPage/>},
            {path: "RecentChange", element: <RecentChangesPage/>},
            {path: "RecentChanges", element: <RecentChangesPage/>},
            {path: "S3", element: <S3BrowserPage/>},
            {path: "S3Browser", element: <S3BrowserPage/>},
            {path: "CrawlerCache", element: <CrawlerCachePage/>},
            {path: "CrawlerCaches", element: <CrawlerCachePage/>},
        ],
    },
]);

window.addEventListener("error", (event) => console.error("[AdminUI] window:error", event.message, event.error));
window.addEventListener("unhandledrejection", (event) => console.error("[AdminUI] window:unhandledrejection", event.reason));

window.addEventListener("DOMContentLoaded", () => {
    const rootElement = document.getElementById("main");
    if (!rootElement) { console.error("[AdminUI] #main not found"); return; }
    createRoot(rootElement).render(
        <MantineProvider defaultColorScheme="light" theme={{primaryColor: "indigo", defaultRadius: "md"}}>
            <RouterProvider router={router}/>
        </MantineProvider>
    );
});
