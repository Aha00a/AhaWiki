export const CRAWLER_CACHE_PAGE_SIZE = 20;
export const ADMIN_PAGE_META_PAGE_SIZE = 20;
export const ACCESS_LOG_PAGE_SIZE = 20;

export const PERMISSION_TARGET_TYPE_OPTIONS = ["All", "Exact", "StartsWith", "EndsWith", "RegularExpression"];
export const PERMISSION_ACTOR_TYPE_OPTIONS = ["All", "Login", "Exact", "Domain"];
export const PERMISSION_ACTION_DEFINITIONS = [
    {value: "None", action: 0},
    {value: "Read", action: 1},
    {value: "Edit", action: 2},
    {value: "Create", action: 4},
    {value: "Upload", action: 8},
    {value: "Delete", action: 16},
    {value: "Admin", action: 255},
];
