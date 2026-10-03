#!/usr/bin/env bash
# Renders every page of a site on two instances and says what differs: the check between the
# canary of a deploy and the instance still on the old code. See docs/ahawiki.net/'Dev Deploying'
# for where it sits in a deploy.
#
#   AHAWIKI_DEPLOY_HOST=<ssh host>  bash scripts/compare-instances.sh <old port> <new port> [site]
#
# The site defaults to ahawiki.net, the wiki this repository mirrors. Its page list comes from
# the old instance's /api/pageNames, and each page's view and history are compared.
#
# The instances answer only on the server's loopback, so the fetching runs there, over one ssh
# connection. Loopback is always whitelisted, so hundreds of page requests do not trip the rate
# limiter the way they would from outside.
#
# Exit status: 0 when nothing changed, 1 when something did -- which may well be the change being
# deployed, so read the report -- and 2 when there is nothing to compare, such as both instances
# running the same release.
#
# Until this existed each canary comparison was written again by hand, and the first attempt on
# 2026-09-26 called 226 of 231 pages different: the adjacent-pages graph draws a new UUID on every
# render. Most of what this script does is set aside what two renders of the same code already
# disagree on, so that what is left is what the deploy changed.

set -euo pipefail

HOST="${AHAWIKI_DEPLOY_HOST:-}"
[ -n "$HOST" ] || { echo "AHAWIKI_DEPLOY_HOST is not set. See docs/ahawiki.net/'Dev Deploying'." >&2; exit 2; }
[ $# -ge 2 ] && [ $# -le 3 ] || { echo "usage: bash scripts/compare-instances.sh <old port> <new port> [site]" >&2; exit 2; }
OLD="$1"
NEW="$2"
SITE="${3:-ahawiki.net}"
# All three go into commands the server's shell reads, so they have to be what they claim to be.
[[ "$OLD" =~ ^[0-9]+$ && "$NEW" =~ ^[0-9]+$ && "$OLD" != "$NEW" ]] || { echo "the ports must be two different numbers" >&2; exit 2; }
[[ "$SITE" =~ ^[A-Za-z0-9.-]+(:[0-9]+)?$ ]] || { echo "not a host name: $SITE" >&2; exit 2; }

# Runs on the server: reads URL paths on stdin and compares what the two ports render for each.
compare_on_server() {
  local old=$1 new=$2 site=$3 path n=0 key
  # Not local: the EXIT trap runs after this function has returned, and a local would be gone by
  # then, leaving the directory behind on the server.
  work=$(mktemp -d) || return 2
  trap 'rm -rf "$work"' EXIT
  cd "$work" || return 2

  # Two renders of the same code already differ in these, so every render has them taken out:
  # every id a template draws fresh on each render is a new UUID (UuidUtil.newString -- the
  # adjacent-pages graph, Kanban, Gantt, maps), an S3 presigned URL carries the moment it was
  # signed, and a template edit moves blank lines and doubled spaces around. An id in any other
  # form, or anything else drawn at random, makes its page differ from itself and drops it from
  # the comparison: maps had both, a dashless id and randomly moved markers, until 2026-10-03.
  canon() {
    sed -E -e 's/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/UUID/g' \
           -e 's/(X-Amz-[A-Za-z]+=)[^&"]*/\1SIGNED/g' \
           -e 's/[[:space:]]+/ /g' -e 's/> />/g; s/ </</g' -e 's/^ //; s/ $//' -e '/^$/d'
  }
  fetch() { curl -s --max-time 60 -H "Host: $site" "http://127.0.0.1:$1$2" | canon; }

  # The old port twice and the new one once. A page the old code renders two ways already says
  # nothing about the new code, so it is set aside rather than called changed. Prints the name of
  # the file the path belongs in, and leaves the renders in a, b and c.
  classify() {
    fetch "$old" "$1" > a
    fetch "$old" "$1" > b
    fetch "$new" "$1" > c
    if cmp -s a c || cmp -s b c; then echo same
    elif ! cmp -s a b; then echo varies
    else echo differs
    fi
  }

  : > same; : > varies; : > differs; : > settled; : > changed
  mkdir changes
  while IFS= read -r path; do
    [ -n "$path" ] || continue
    n=$((n + 1))
    echo "$path" >> "$(classify "$path")"
  done

  # A page can change between the fetches. On 2026-09-26 the similar-pages term lists of three
  # pages were recalculated in the middle of the comparison, and they rendered alike on both
  # instances a minute later. So every difference is looked at once more before it counts.
  while IFS= read -r path; do
    case $(classify "$path") in
      same) echo "$path" >> settled ;;
      varies) echo "$path" >> varies ;;
      differs)
        # Grouped by what changed, so a change every page shares prints once.
        diff a c | grep '^[<>]' > hunk || true
        key=$(md5sum < hunk | cut -c1-12)
        [ -e "changes/$key" ] || cp hunk "changes/$key"
        echo "$key $path" >> changed
        ;;
    esac
  done < differs

  echo "$n paths on $site: $(wc -l < same) same, $(wc -l < settled) same on a second look," \
       "$(wc -l < varies) differ between two renders of the old code, $(wc -l < changed) changed"

  if [ -s varies ]; then
    echo
    echo "differ between two renders of the old code, so they say nothing about the new one:"
    sed 's/^/  /' varies
  fi

  if [ -s changed ]; then
    echo
    echo "changed, grouped by what changed ('<' is the old port, '>' the new):"
    cut -d' ' -f1 changed | sort | uniq -c | sort -rn | while read -r count key; do
      echo "  on $count path(s): $(grep "^$key " changed | cut -d' ' -f2- | head -3 | paste -sd' ')$([ "$count" -gt 3 ] && echo ' ...')"
      head -20 "changes/$key" | cut -c1-200 | sed 's/^/      /'
    done
    return 1
  fi
  return 0
}

# Which release each instance started from. The unit's working directory is `current`, resolved
# when the process started, so it names the release even after `current` has moved on.
# Each line carries its port, so an instance that cannot be read leaves its own line empty
# instead of shifting the other one into its place.
releases=$(ssh "$HOST" "for p in $OLD $NEW; do echo \"\$p \$(sudo -n readlink -f /proc/\$(systemctl show ahawiki@\$p -p MainPID --value)/cwd)\"; done") || true
old_release=$(awk -v p="$OLD" '$1 == p { print $2 }' <<< "$releases")
new_release=$(awk -v p="$NEW" '$1 == p { print $2 }' <<< "$releases")
echo "old $OLD: ${old_release:-?}"
echo "new $NEW: ${new_release:-?}"
if [ -z "$old_release" ] || [ -z "$new_release" ]; then
  echo "could not tell which release an instance runs" >&2
  exit 2
fi
if [ "$old_release" = "$new_release" ]; then
  echo "both run the same release, so there is nothing to compare" >&2
  exit 2
fi

page_names=$(ssh "$HOST" "curl -s --max-time 60 -H 'Host: $SITE' http://127.0.0.1:$OLD/api/pageNames") || true
[ -n "$page_names" ] || { echo "$OLD did not answer /api/pageNames for $SITE" >&2; exit 2; }
mapfile -t paths < <(node -e '
  const names = JSON.parse(require("fs").readFileSync(0, "utf8"));
  for (const n of names) console.log("/w/" + encodeURIComponent(n));
  for (const n of names) console.log("/w/" + encodeURIComponent(n) + "?action=history");
' <<< "$page_names")
[ "${#paths[@]}" -gt 0 ] || { echo "the old instance listed no pages for $SITE" >&2; exit 2; }

# The paths go in a quoted here-document so the server's shell reads them as data, not code.
{
  declare -f compare_on_server
  printf "compare_on_server %q %q %q <<'PATHS'\n" "$OLD" "$NEW" "$SITE"
  printf '%s\n' "${paths[@]}"
  echo PATHS
} | ssh "$HOST" bash -s
