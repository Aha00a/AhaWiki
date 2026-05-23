import React, {useEffect, useState} from "react";
import {useNavigate} from "react-router-dom";
import {SiteListCard} from "../../site/siteWidgets.jsx";
import {fetchJson, logError} from "../api.js";

export default function SitesPage() {
    const navigate = useNavigate();
    const [sites, setSites] = useState([]);

    useEffect(() => {
        fetchJson("/api/Admin/Sites").then(setSites).catch((err) => logError("sites:load:error", err));
    }, []);

    return <SiteListCard sites={sites} onNavigate={(href) => navigate(href)}/>;
}
