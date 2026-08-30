import dayjs from "dayjs";

export function toPascalCase(value) {
    if (!value) return "";
    return String(value).replace(/(^|[-_\s]+)([a-z0-9])/g, (_match, _separator, character) => character.toUpperCase());
}

/**
 * Select options for the permission `action` field, from the list the server sent.
 *
 * The option value is the action's name, which is what the save and diagnose endpoints
 * parse; the number is shown only so the label matches the row list, which prints the same
 * "32 - Delete" shape.
 */
export function toPermissionActionOptions(actions) {
    return (actions ?? []).map(({name, action}) => ({
        value: name,
        label: `${action} - ${toPascalCase(name)}`,
    }));
}

/** Both halves come from the row itself; nothing here has an opinion about the numbering. */
export function formatPermissionAction(actionName, action) {
    const label = toPascalCase(actionName || String(action ?? ""));
    return action === undefined || action === null || action === "" ? label : `${action} - ${label}`;
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
