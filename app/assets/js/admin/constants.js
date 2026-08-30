export const CRAWLER_CACHE_PAGE_SIZE = 20;
export const ADMIN_PAGE_META_PAGE_SIZE = 20;
export const ACCESS_LOG_PAGE_SIZE = 20;

// The permission targetType / actorType / action vocabularies used to be listed here. They
// are Permission's enumerations, and the copy drifted from them — Rename went missing and
// Upload and Delete were off by one step each. GET /api/Admin/Site/:seq/Permissions serves
// them alongside the rows now.
