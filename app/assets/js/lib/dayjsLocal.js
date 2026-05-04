export default function dayjs(value) {
    const date = new Date(value);
    return {
        isValid() {
            return !Number.isNaN(date.getTime());
        },
        format(pattern) {
            if (pattern !== "YYYY-MM-DDTHH:mm:ssZ") {
                throw new Error(`Unsupported format: ${pattern}`);
            }
            const pad = (num) => String(num).padStart(2, "0");
            const year = date.getFullYear();
            const month = pad(date.getMonth() + 1);
            const day = pad(date.getDate());
            const hours = pad(date.getHours());
            const minutes = pad(date.getMinutes());
            const seconds = pad(date.getSeconds());

            const offsetMinutes = -date.getTimezoneOffset();
            const sign = offsetMinutes >= 0 ? "+" : "-";
            const absOffsetMinutes = Math.abs(offsetMinutes);
            const offsetHours = pad(Math.floor(absOffsetMinutes / 60));
            const offsetMins = pad(absOffsetMinutes % 60);

            return `${year}-${month}-${day}T${hours}:${minutes}:${seconds}${sign}${offsetHours}:${offsetMins}`;
        },
    };
}
