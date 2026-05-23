import dayjs from "dayjs";
import {PERMISSION_ACTION_DEFINITIONS} from "./constants.js";

export function toPascalCase(value) {
    if (!value) return "";
    return String(value).replace(/(^|[-_\s]+)([a-z0-9])/g, (_match, _separator, character) => character.toUpperCase());
}

export const PERMISSION_ACTION_OPTIONS = PERMISSION_ACTION_DEFINITIONS.map(({value, action}) => ({
    value,
    label: `${action} - ${toPascalCase(value)}`,
}));

export function formatPermissionAction(actionName, action) {
    const definition = PERMISSION_ACTION_DEFINITIONS.find((item) => item.value === actionName || item.action === action);
    const resolvedActionName = actionName || definition?.value || String(action ?? "");
    const resolvedAction = action ?? definition?.action;
    const label = toPascalCase(resolvedActionName);
    return resolvedAction === undefined || resolvedAction === null || resolvedAction === "" ? label : `${resolvedAction} - ${label}`;
}

export function compareValuesForSort(leftValue, rightValue, direction) {
    const directionMultiplier = direction === "desc" ? -1 : 1;
    const normalizedLeft = leftValue ?? "";
    const normalizedRight = rightValue ?? "";
    if (typeof normalizedLeft === "number" && typeof normalizedRight === "number") {
        return (normalizedLeft - normalizedRight) * directionMultiplier;
    }
    return String(normalizedLeft).localeCompare(String(normalizedRight), undefined, {numeric: true, sensitivity: "base"}) * directionMultiplier;
}

export function formatDateTimeInClientTimezone(value) {
    if (!value) return "-";
    const parsed = dayjs(value);
    if (!parsed.isValid()) return value;
    return parsed.format("YYYY-MM-DDTHH:mm:ssZ");
}

export function formatCrawlerStatus(value) {
    if (!value) return "-";
    if (typeof value === "string") return value;
    if (typeof value === "object") {
        const keys = Object.keys(value);
        if (keys.length > 0) return keys.join(", ");
    }
    return String(value);
}

export function resolveSiteUrl(row, siteDomainBySeq) {
    const domainFromRow = typeof row.siteDomain === "string" ? row.siteDomain.trim() : "";
    const domainFromSites = (siteDomainBySeq?.get(row.siteSeq) ?? "").trim();
    const domain = domainFromRow || domainFromSites;
    return domain ? `https://${domain}` : "";
}
