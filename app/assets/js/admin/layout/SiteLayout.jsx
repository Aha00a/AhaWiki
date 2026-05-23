import React from "react";
import {Outlet, useParams} from "react-router-dom";
import {useAdminContext} from "../context/AdminContext.jsx";
import {useSiteData} from "../hooks/useSiteData.js";

export default function SiteLayout() {
    const {siteSeq} = useParams();
    const {me} = useAdminContext();
    const {site, sitePageNames, saveSiteMeta, savingSiteMeta, refreshPageNames, error} = useSiteData(siteSeq);

    return <Outlet context={{site, siteSeq, sitePageNames, me, refreshPageNames, saveSiteMeta, savingSiteMeta, error}}/>;
}
