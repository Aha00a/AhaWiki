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

    const WikipediaToSchemaProperty = {
        "Address": "address",
        "Alma mater": "alumniOf",
        "Also known as": "alternateName",
        "Application category": "applicationCategory",
        "Application subcategory": "applicationSubCategory",
        "Area served": "areaServed",
        "Author": "author",
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
        "Written by": "author",
        "Written in": "programmingLanguage",
        "각본": "author",
        "개발자": "author",
        "기획": "producer",
        "라이선스": "license",
        "발표일": "datePublished",
        "방송 국가": "countryOfOrigin",
        "방송 기간": "startDate",
        "방송 채널": "publisher",
        "방송 횟수": "numberOfEpisodes",
        "상태": "status",
        "언어": "inLanguage",
        "연출": "director",
        "운영 체제": "operatingSystem",
        "웹사이트": "url",
        "음악": "musicBy",
        "장르": "genre",
        "제작": "producer",
        "제작사": "productionCompany",
        "조연출": "director",
        "종류": "applicationCategory",
        "추가 채널": "publisher",
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

    function convertWikipediaToSchemaOrg(arrayArrayValue, removeOriginal) {
        return arrayArrayValue
            .flatMap(([head, ...rest]) => [
                removeOriginal || ["# " + head, ...rest],
                [convertProperty(normalizeProp(head)), ...(rest.map(normalizeValue))],
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
        WikipediaToSchemaProperty,
    };
})();
