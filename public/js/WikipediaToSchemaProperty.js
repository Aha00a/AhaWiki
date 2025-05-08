(() => {
    const WikipediaToSchemaProperty = {
        "Address": "address",
        "Alma mater": "alumniOf",
        "Application category": "applicationCategory",
        "Application subcategory": "applicationSubCategory",
        "Author": "author",
        "Awards": "award",
        "Based on": "isBasedOn",
        "Book edition": "bookEdition",
        "Book format": "bookFormat",
        "Born": "birthDate",
        "Brand": "brand",
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
        "Developer(s)": "author",
        "Diagnosis": "diagnosis",
        "Died": "deathDate",
        "Dimensions": "depth",
        "Directed by": "director",
        "Distributed by": "publisher",
        "Edited by": "editor",
        "Executive producers": "producer",
        "Founded": "foundingDate",
        "Founder": "founder",
        "Genre": "genre",
        "Headquarters": "location",
        "ICD-10": "code",
        "Illustrator": "illustrator",
        "Industry": "industry",
        "Initial release": "datePublished",
        "ISBN": "isbn",
        "Key people": "employee",
        "Language": "inLanguage",
        "License": "license",
        "Location": "location",
        "Manufacturer": "manufacturer",
        "Model": "model",
        "Music by": "musicBy",
        "Nationality": "nationality",
        "Network": "publisher",
        "No. of episodes": "numberOfEpisodes",
        "No. of seasons": "numberOfSeasons",
        "Occupation": "jobTitle",
        "Operating system": "operatingSystem",
        "Organizer": "organizer",
        "Original author(s)": "creator",
        "Original language": "inLanguage",
        "Parents": "parent",
        "Platform": "runtimePlatform",
        "Price": "offers",
        "Produced by": "producer",
        "Producers": "producer",
        "Production companies": "productionCompany",
        "Production company": "productionCompany",
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
        "Spouse(s)": "spouse",
        "Starring": "actor",
        "Treatments": "possibleTreatment",
        "Type": "applicationCategory",
        "Version": "softwareVersion",
        "Website": "url",
        "Weight": "weight",
        "Written by": "author",
        "Written in": "programmingLanguage",
        "각본": "author",
        "개발자": "author",
        "라이선스": "license",
        "발표일": "datePublished",
        "방송 국가": "countryOfOrigin",
        "방송 채널": "publisher",
        "상태": "status",
        "언어": "inLanguage",
        "연출": "director",
        "운영 체제": "operatingSystem",
        "웹사이트": "url",
        "제작": "producer",
        "제작사": "productionCompany",
        "종류": "applicationCategory",
        "출연자": "actor",
        "크기": "size",
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
                [convertProperty(head), ...(rest.map(normalizeValues))],
            ].filter(_ => _))
            .map(l => l.join('\t'))
            .join('\n');
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
    function convertEnglishDates(text) {
        return text.replace(/\b([A-Z][a-z]+)\s+(\d{1,2}),\s+(\d{4})\b/g, (match, month, day, year) => {
            const mm = months[month];
            if (!mm)
                return match;

            const dd = day.padStart(2, '0');
            return `${year}-${mm}-${dd}`;
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

    function normalizeValues(text) {
        return [
            convertEnglishDates,
            convertKoreanDates,
            convertCountryNames,
            convertLanguageNames,
            convertPeriodsToISO,
        ].reduce((acc, fn) => fn(acc), text);
    }



    if(typeof window.AhaWiki === "undefined") {
        window.AhaWiki = {};
    }

    window.AhaWiki.WikipediaToSchema = {
        isPropertyDefined,
        convertProperty,
        convertWikipediaToSchemaOrg,
        convertEnglishDates,
        convertKoreanDates,
        convertCountryNames,
        convertLanguageNames,
        convertPeriodsToISO,
        normalizeValues,
        WikipediaToSchemaProperty,
    };
})();
