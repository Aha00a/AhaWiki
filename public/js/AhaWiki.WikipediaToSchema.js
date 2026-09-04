(() => {
    function getApiUrlWikipedia(lang, page) {
        return `https://${lang}.wikipedia.org/api/rest_v1/page/html/${encodeURIComponent(page)}?origin=*`;
    }

    function getUrlWikipedia(lang, page) {
        return `https://${lang}.wikipedia.org/wiki/${encodeURIComponent(page)}`;
    }

    async function fetchInfoBoxFromWikipedia(lang, page) {
        const url = getApiUrlWikipedia(lang, page);
        const arrayArrayValue = [];
        try {
            const res = await fetch(url);
            if (!res.ok)
                return arrayArrayValue;

            const html = await res.text();
            const parser = new DOMParser();
            const doc = parser.parseFromString(html, 'text/html');

            const styleTags = doc.querySelectorAll('style');
            styleTags.forEach(tag => tag.remove());

            const hiddenSpans = doc.querySelectorAll('span[style*="display:none;"], span[style*="display: none;"]');
            hiddenSpans.forEach(span => span.remove());

            const infobox = doc.querySelector('.infobox');
            if (!infobox)
                return arrayArrayValue;

            const captions = infobox.querySelectorAll('caption');
            for (const caption of captions) {
                const text = caption.innerText.trim();
                if (text) {
                    arrayArrayValue.push(['name', text]);
                }
            }

            const rows = infobox.querySelectorAll('tr');

            for (const row of rows) {
                const thInfoboxAboveSummary = row.querySelector('th.infobox-above');
                if(thInfoboxAboveSummary) {
                    arrayArrayValue.push(['name', thInfoboxAboveSummary.innerText]);
                    continue;
                }

                const tdInfoboxImageImg = row.querySelector('td.infobox-image img');
                if(tdInfoboxImageImg) {
                    arrayArrayValue.push(['image', tdInfoboxImageImg.src.replace(/^http:/, 'https:')]);
                    continue;
                }

                const th = row.querySelector('th');
                const td = row.querySelector('td');

                if (th && td) {
                    const key = th.innerText.trim().replace(/\s+/g, ' ');
                    const tdLi = td.querySelectorAll('li');
                    const values = tdLi.length
                        ? [...tdLi].map(v => v.innerText.trim())
                        : td.innerText.trim().split(/\n+/g).flatMap(v => v.split(',').map(v => v.trim()).filter(v => v));
                    const line = [key, ...values];
                    arrayArrayValue.push(line);
                }
            }

            return arrayArrayValue;
        } catch (e) {
            console.log(`${lang}`, e);
            return arrayArrayValue;
        }
    }

    /**
     * 위키백과 infobox 의 행 이름 → schema.org 속성.
     *
     * 여기 없는 이름은 그대로 통과한다(`convertProperty`). 그래서 '''틀린 매핑을 넣는 것이
     * 안 넣는 것보다 나쁘다''' — 통과한 이름은 사람이 보고 고치지만, 잘못 매핑된 이름은 맞는
     * 것처럼 보인다.
     *
     * ⓘ '''소유(Owner·소유자)는 일부러 비워 두었다.''' schema.org 에 장소의 소유자를 가리키는
     * 속성이 없다. 가장 가까운 `owns` 는 range 가 Product 이고 domain 이 Person·Organization 이라
     * "사람/조직이 소유한 물건" — 방향이 반대다. `ownedFrom`·`ownedThrough` 는 OwnershipInfo
     * 라는 별도 구조의 것이고, `additionalProperty` 는 중첩 객체라 이 평평한 표로는 못 만든다.
     * 그래서 Owner 는 매핑하지 않고 통과시킨다. 채워 넣지 말 것. (2026-09-04 26.0 어휘 기준)
     *
     * ⓘ '''`Country` 는 애매하다.''' 여기서는 `countryOfOrigin`(Product·CreativeWork) 으로 둔다 —
     * 영화·드라마 infobox 가 압도적으로 많아서다. 건물·장소라면 `addressCountry` 가 맞지만,
     * 이 표는 행 이름만 보고 페이지의 타입을 모른다. 장소인 것이 확실한 이름들(`소재지` 등)만
     * 주소 계열로 보낸다.
     */
    const WikipediaToSchemaProperty = {
        "Abbreviation": "alternateName",
        "Address": "address",
        "Alma mater": "alumniOf",
        "Also known as": "alternateName",
        "Application category": "applicationCategory",
        "Application subcategory": "applicationSubCategory",
        "Area served": "areaServed",
        "Author": "author",
        "Available in": "inLanguage",
        "Awards": "award",
        "Based on": "isBasedOn",
        "Book edition": "bookEdition",
        "Book format": "bookFormat",
        "Born": "birthDate",
        "Brand": "brand",
        "Brands": "brand",
        "Children": "children",
        "City": "addressLocality",
        "Composer": "musicBy",
        "Coordinates": "geo",
        "Countries": "countryOfOrigin",
        "Country of origin": "countryOfOrigin",
        "Country": "countryOfOrigin",
        "Created by": "creator",
        "Date": "startDate",
        "Developer": "author",
        "Diagnosis": "diagnosis",
        "Died": "deathDate",
        "Dimensions": "depth",
        "Directed by": "director",
        "Distributed by": "publisher",
        "Divisions": "department",
        "Edited by": "editor",
        "Editor": "editor",
        "Executive producer": "producer",
        "Executive producers": "producer",
        "Formation": "foundingDate",
        "Founded": "foundingDate",
        "Founder": "founder",
        "Founders": "founder",
        "Genre": "genre",
        "Genres": "genre",
        "Headquarters": "location",
        "ICD-10": "code",
        "ISBN": "isbn",
        "Illustrator": "illustrator",
        "Industry": "industry",
        "Initial release": "datePublished",
        "Key people": "employee",
        "Language": "inLanguage",
        "Launched": "datePublished",
        "Licence": "license",
        "License": "license",
        "Location": "location",
        "Parent": "parentOrganization",
        "Parent company": "parentOrganization",
        "Manufacturer": "manufacturer",
        "Member of": "memberOf",
        "Model": "model",
        "Music by": "musicBy",
        "Nationality": "nationality",
        "Network": "publisher",
        "No. of episodes": "numberOfEpisodes",
        "No. of seasons": "numberOfSeasons",
        "Occupation": "jobTitle",
        "Occupations": "hasOccupation",
        "Operating system": "operatingSystem",
        "Organizer": "organizer",
        "Origin": "birthPlace",
        "Original author": "creator",
        "Original language": "inLanguage",
        "Other names": "alternateName",
        "Parent organization": "parentOrganization",
        "Parents": "parent",
        "Platform": "runtimePlatform",
        "Price": "offers",
        "Producer": "producer",
        "Produced by": "producer",
        "Producers": "producer",
        "Production companies": "productionCompany",
        "Production company": "productionCompany",
        "Production location": "contentLocation",
        "Production locations": "contentLocation",
        "Productioncompanies": "productionCompany",
        "Productioncompany": "productionCompany",
        "Region": "addressRegion",
        "Release date": "datePublished",
        "Release dates": "datePublished",
        "Release": "startDate",
        "Repository": "codeRepository",
        "Risk factors": "riskFactor",
        "Running time": "duration",
        "Spouses": "spouse",
        "Stable release": "softwareVersion",
        "Starring": "actor",
        "Subsidiaries": "subOrganization",
        "Treatments": "possibleTreatment",
        "Type": "applicationCategory",
        "URL": "url",
        "Version": "softwareVersion",
        "Website": "url",
        "Weight": "weight",
        // schema.org has no `writer`, and no screenwriter property either -- `author` is what a
        // film's or series' writer gets. 137 pages on the wiki had been given a lowercase
        // `writer` by hand, which looks like a property and is not one; these labels are why.
        "Writer": "author",
        "Writers": "author",
        "Written by": "author",
        "Screenplay": "author",
        "Screenplay by": "author",
        "Story by": "author",
        "Written in": "programmingLanguage",
        "Year started": "datePublished",
        "Street address": "streetAddress",
        "Postal code": "postalCode",
        "Postcode": "postalCode",
        "ZIP code": "postalCode",
        "Town or city": "addressLocality",
        "State": "addressRegion",
        "Province": "addressRegion",
        "District": "addressRegion",
        "Completed": "yearBuilt",
        "Completion": "yearBuilt",
        "Year built": "yearBuilt",
        "Built": "yearBuilt",
        "주소": "address",
        "소재지": "address",
        "위치": "address",
        "도시": "addressLocality",
        "우편번호": "postalCode",
        "좌표": "geo",
        // ko.wikipedia's building infobox writes this, not 좌표 -- 롯데월드타워 and 63빌딩 both
        // do, and the row was passing through untouched while the value was already parseable.
        "지리 좌표계": "geo",
        // foundingDate's range is Date, so unlike yearBuilt this keeps the month and day.
        "설립일": "foundingDate",
        "완공": "yearBuilt",
        "준공": "yearBuilt",
        "완공일": "yearBuilt",
        "준공일": "yearBuilt",
        "각본": "author",
        "극본": "author",
        "작가": "author",
        "감독": "director",
        "개발자": "author",
        "개봉일": "datePublished",
        "국가": "countryOfOrigin",
        "기획": "producer",
        "라이선스": "license",
        "발표일": "datePublished",
        "방송 국가": "countryOfOrigin",
        "방송 기간": "startDate",
        "방송 채널": "publisher",
        "방송 횟수": "numberOfEpisodes",
        "배급사": "publisher",
        "상태": "status",
        "시간": "duration",
        "언어": "inLanguage",
        "연출": "director",
        "운영 체제": "operatingSystem",
        "원작": "isBasedOn",
        "웹사이트": "url",
        "음악": "musicBy",
        "장르": "genre",
        "제작": "producer",
        "제작사": "productionCompany",
        "조연출": "director",
        "종류": "applicationCategory",
        "책임프로듀서": "producer",
        "추가 채널": "publisher",
        "출연": "actor",
        "출연자": "actor",
        "크기": "size",
        "편집": "editor",
        "프로듀서": "producer",
    };

    function isPropertyDefined(property) {
        return WikipediaToSchemaProperty.hasOwnProperty(property);
    }

    function convertProperty(property) {
        return WikipediaToSchemaProperty[property] ?? property
    }

    /**
     * 한 줄이 여러 줄이 되기도 한다.
     *
     * `geo` 는 range 가 GeoCoordinates·GeoShape 인 '''객체''' 라서, 좌표 문자열을 거기 넣으면
     * 타입이 맞지 않는다. 읽어낼 수 있으면 `latitude`·`longitude`(둘 다 Place, range Text|Number)
     * 두 줄로 나눈다. 못 읽으면 원래대로 `geo` 한 줄을 둔다 — 사람이 보고 고칠 수 있게.
     */
    function toSchemaLines(property, values) {
        if (property === 'geo') {
            const coordinates = parseCoordinates(values.join(' '));
            if (coordinates)
                return [['latitude', String(coordinates.latitude)], ['longitude', String(coordinates.longitude)]];
        }
        const transform = SchemaPropertyRowTransform[property];
        return [[property, ...(transform ? transform(values) : values)]];
    }

    function convertWikipediaToSchemaOrg(arrayArrayValue, removeOriginal) {
        return arrayArrayValue
            .flatMap(([head, ...rest]) => [
                // `removeOriginal || [...]` 였는데, true 를 주면 배열이 아니라 true 가 통과해
                // 아래 join 에서 터졌다. 부르는 곳이 인자를 넘기지 않아 드러나지 않았을 뿐이다.
                removeOriginal ? null : ["# " + head, ...rest],
                ...toSchemaLines(convertProperty(normalizeProp(head)), rest.map(normalizeValue)),
                [],
            ].filter(_ => _))
            .map(l => l.join('\t'))
            .join('\n');
    }



    function normalizeProp(text) {
        return text.replace(/\(s\)/g, '')
    }


    const months = {
        January: '01', February: '02', March: '03',
        April: '04', May: '05', June: '06',
        July: '07', August: '08', September: '09',
        October: '10', November: '11', December: '12',
        Jan: '01', Feb: '02', Mar: '03', Apr: '04',
        Jun: '06', Jul: '07', Aug: '08', Sep: '09',
        Oct: '10', Nov: '11', Dec: '12'
    };

    // Pattern for "Month Day[,| ] Year" e.g., March 3, 2020 or March 3 2020
    const regexMonthDayYear = new RegExp([
        '\\b',                   // Word boundary
        '([A-Z][a-z]+)',         // Group 1: Month (capitalized word)
        '\\s+',
        '(\\d{1,2})',            // Group 2: Day (1 or 2 digits)
        ',?',                   // Optional comma
        '\\s+',
        '(\\d{4})',              // Group 3: Year (4 digits)
        '\\b'
    ].join(''), 'g');

    // Pattern for "Day Month Year" e.g., 3 March 2020
    const regexDayMonthYear = new RegExp([
        '\\b',
        '(\\d{1,2})',            // Group 1: Day
        '\\s+',
        '([A-Z][a-z]+)',         // Group 2: Month
        '\\s+',
        '(\\d{4})',              // Group 3: Year
        '\\b'
    ].join(''), 'g');

    function formatDate(year, month, day) {
        const mm = months[month];
        if (!mm) return null;
        const dd = day.padStart(2, '0');
        return `${year}-${mm}-${dd}`;
    }

    function convertEnglishDates(text) {
        return text
            .replace(regexMonthDayYear, (match, month, day, year) => {
                const formatted = formatDate(year, month, day);
                return formatted || match;
            })
            .replace(regexDayMonthYear, (match, day, month, year) => {
                const formatted = formatDate(year, month, day);
                return formatted || match;
            });
    }

    function convertKoreanDates(text) {
        return text.replace(/(\d{4})년\s*(\d{1,2})월\s*(\d{1,2})일/g, (match, year, month, day) => {
            return `${year}-${month.padStart(2, '0')}-${day.padStart(2, '0')}`;
        });
    }

    const countryMap = {
        "Australia": "AU",
        "Brazil": "BR",
        "Canada": "CA",
        "China": "CN",
        "France": "FR",
        "Germany": "DE",
        "India": "IN",
        "Japan": "JP",
        "Mexico": "MX",
        "North Korea": "KP",
        "Russia": "RU",
        "South Korea": "KR",
        "United Kingdom": "GB",
        "United States": "US",
    };
    function convertCountryNames(text) {
        const pattern = new RegExp('\\b(' + Object.keys(countryMap).join('|') + ')\\b', 'g');
        return text.replace(pattern, match => countryMap[match]);
    }

    const languageMap = {
        "Arabic": "ar",
        "Chinese": "zh",
        "Dutch": "nl",
        "English": "en",
        "French": "fr",
        "German": "de",
        "Hindi": "hi",
        "Italian": "it",
        "Japanese": "ja",
        "Korean": "ko",
        "Portuguese": "pt",
        "Russian": "ru",
        "Spanish": "es",
        "Turkish": "tr",
        "Vietnamese": "vi",
    };
    function convertLanguageNames(text) {
        const pattern = new RegExp('\\b(' + Object.keys(languageMap).join('|') + ')\\b', 'g');
        return text.replace(pattern, match => languageMap[match]);
    }

    function convertPeriodsToISO(text) {
        return text.replace(/((\d+)\s*(hours?|minutes?|seconds?))/g, '|||$1')
            .split('|||')
            .map(part => {
                const matches = [...part.matchAll(/(\d+)\s*(hours?|minutes?|seconds?)/g)];
                if (matches.length === 0) return part;
                const isoParts = matches.map(([, value, unit]) => {
                    if (unit.includes('hour')) return `${value}H`;
                    if (unit.includes('minute')) return `${value}M`;
                    if (unit.includes('second')) return `${value}S`;
                    return '';
                });
                return `PT${isoParts.join('')}`;
            })
            .join('');
    }

    function removeBracketDigitBracket(text) {
        return text.replace(/\s*\[\d+]\s*/g, ' ').replace(/\s+/g, ' ').trim();
    }

    // 위도/경도 한 짝. Wikipedia 좌표는 도분초("37°33′36″N 126°58′41″E"), 도분("37°33′N"),
    // 십진수("37.5665, 126.9780") 가 섞여 나오고 기호도 ′″ 와 '" 가 섞인다.
    // The hemisphere is written after the number in English ("37°33′36″N") and before it in
    // Korean ("북위 37° 34′ 21″"), so both ends are optional here and a match with neither is
    // discarded below. Reading only the trailing letter left every ko.wikipedia coordinate
    // unparsed, and the row stayed as a single geo line.
    const regexCoordinateDms = new RegExp([
        '(북위|남위|동경|서경)?\\s*',             // 반구 (한국어, 앞에 온다)
        '(\\d+(?:\\.\\d+)?)\\s*[°º]',            // 도
        '(?:\\s*(\\d+(?:\\.\\d+)?)\\s*[′\'’])?', // 분 (없을 수 있다)
        '(?:\\s*(\\d+(?:\\.\\d+)?)\\s*[″"”])?',  // 초 (없을 수 있다)
        '\\s*([NSEW])?',                         // 반구 (라틴, 뒤에 온다)
    ].join(''), 'gi');

    const KoreanHemisphere = {'북위': 'N', '남위': 'S', '동경': 'E', '서경': 'W'};

    // Not anchored: Wikipedia often ends a coordinate cell with a clean decimal pair after the
    // sexagesimal forms ("... / 37.5725; 126.9756"), and requiring the whole cell to be the pair
    // threw that away.
    const regexCoordinateDecimalPair = /(-?\d+\.\d+)\s*[,;/]\s*(-?\d+\.\d+)/;

    function dmsToDecimal(degrees, minutes, seconds, hemisphere) {
        const decimal = Number(degrees) + Number(minutes || 0) / 60 + Number(seconds || 0) / 3600;
        // 소수점 6자리면 약 0.1m 이다. 그 아래는 위키백과 원문에 없는 정밀도다.
        const rounded = Math.round(decimal * 1e6) / 1e6;
        return /[SW]/i.test(hemisphere) ? -rounded : rounded;
    }

    /** "37°33′36″N 126°58′41″E" → {latitude, longitude}. 못 읽으면 null. */
    // Sexagesimal first, decimal second: when a cell carries both, the degrees/minutes/seconds
    // form is what the article actually states and the decimal tail is its rounding.
    function parseCoordinates(text) {
        const values = [...String(text).matchAll(regexCoordinateDms)]
            .map(([, korean, d, m, s, latin]) => ({
                value: dmsToDecimal(d, m, s, korean ? KoreanHemisphere[korean] : latin),
                hemisphere: korean ? KoreanHemisphere[korean] : (latin || '').toUpperCase(),
            }))
            .filter(v => v.hemisphere);

        const latitude = values.find(v => v.hemisphere === 'N' || v.hemisphere === 'S');
        const longitude = values.find(v => v.hemisphere === 'E' || v.hemisphere === 'W');
        if (latitude && longitude)
            return {latitude: latitude.value, longitude: longitude.value};

        const decimalPair = String(text).match(regexCoordinateDecimalPair);
        if (decimalPair)
            return {latitude: Number(decimalPair[1]), longitude: Number(decimalPair[2])};

        return null;
    }

    /**
     * 속성마다 값의 타입이 다르다. schema.org 의 range 를 따르는 자리.
     *
     * `yearBuilt` 는 range 가 Number(연도)라 "1988-05-10" 을 그대로 두면 타입이 어긋난다.
     * 값에서 4자리 연도만 뽑는다 — 뽑지 못하면 원문을 그대로 두어 사람이 보게 한다.
     */
    // A single infobox cell can arrive as several values: Wikipedia splits
    // "April 11, 1931; 95 years ago (1931-04-11)" on its comma, so the day and the year land
    // apart. A transform looking at one value at a time cannot tell that from two separate
    // answers, and yearBuilt came out as "April 11<TAB>1931". These get the whole row.
    // Mapping targets whose schema.org range is Date. A test derives this list from the shipped
    // vocabulary and fails if the table gains another one, so it cannot quietly fall behind.
    const SchemaDateProperties = ['foundingDate', 'birthDate', 'deathDate', 'datePublished', 'startDate'];

    // Wikipedia writes the machine-readable form in brackets after the prose -- "February 8,
    // 2008(18 years ago) (2008-02-08) (as Logical Awesome LLC)" -- and the comma split scatters
    // the prose across values. Take the ISO date out of the row and drop the rest.
    function toSingleDate(values) {
        const iso = values.join(' ').match(/\b(\d{4}-\d{2}-\d{2})\b/);
        return iso ? [iso[1]] : values;
    }

    const SchemaPropertyRowTransform = {
        // yearBuilt's range is Number, so the row has to collapse to one year. Note this drops
        // the month and day on purpose -- the property cannot hold them. The original line is
        // kept above as a "# " comment, so the full date stays in the page either way.
        yearBuilt: values => {
            const year = values.join(' ').match(/\b(1\d{3}|20\d{2})\b/);
            return year ? [year[1]] : values;
        },
        ...Object.fromEntries(SchemaDateProperties.map(property => [property, toSingleDate])),
    };

    function normalizeValue(text) {
        return [
            convertEnglishDates,
            convertKoreanDates,
            convertCountryNames,
            convertLanguageNames,
            convertPeriodsToISO,
            removeBracketDigitBracket,
        ].reduce((acc, fn) => fn(acc), text);
    }



    if(typeof window.AhaWiki === "undefined") {
        window.AhaWiki = {};
    }

    window.AhaWiki.WikipediaToSchema = {
        getApiUrlWikipedia,
        getUrlWikipedia,
        fetchInfoBoxFromWikipedia,
        isPropertyDefined,
        convertProperty,
        convertWikipediaToSchemaOrg,
        convertEnglishDates,
        convertKoreanDates,
        convertCountryNames,
        convertLanguageNames,
        convertPeriodsToISO,
        normalizeValue,
        parseCoordinates,
        WikipediaToSchemaProperty,
        SchemaDateProperties,
    };
})();
