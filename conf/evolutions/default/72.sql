# --- !Ups

# The admin's recent changes across every site (ApiAdminReport.adminRecentChanges) order all
# revisions of all sites by time. Evolution 71 keyed one site's revisions by time, but that key
# leads with site and cannot serve an order with no site in it, so this screen still sorted the
# whole table. This key is read backward and stops at the LIMIT. isMinorEdit and viaApi follow
# so the filter on them is answered from the key.
CREATE INDEX Page_dateTime_isMinorEdit_viaApi_index
    ON Page (dateTime, isMinorEdit, viaApi);

# --- !Downs

DROP INDEX Page_dateTime_isMinorEdit_viaApi_index ON Page;
