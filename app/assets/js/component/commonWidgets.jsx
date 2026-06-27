import React from "react";
import {Table, Tooltip} from "@mantine/core";

export function IconChevronUp({size = 14}) {
    return (
        <svg width={size} height={size} viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" aria-hidden="true">
            <path d="M18 15l-6-6-6 6"/>
        </svg>
    );
}

export function IconSelector({size = 14}) {
    return (
        <svg width={size} height={size} viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" aria-hidden="true">
            <path d="M8 9l4-4 4 4"/>
            <path d="M16 15l-4 4-4-4"/>
        </svg>
    );
}

const flagIconStyle = {
    display: "flex",
    alignItems: "center",
    justifyContent: "center",
    width: "100%",
    minWidth: 18,
};

export function makeFlagHeader(label, symbol) {
    return (
        <Tooltip label={label} withArrow>
            <span style={flagIconStyle} title={label} aria-label={label}>
                <span aria-hidden="true">{symbol}</span>
            </span>
        </Tooltip>
    );
}

export function makeFlagCell(active, label, symbol, detail = null) {
    if (!active) return "";
    const tooltipLabel = detail ? `${label}: ${detail}` : label;
    return (
        <Tooltip label={tooltipLabel} withArrow>
            <span style={detail ? {...flagIconStyle, gap: 4} : flagIconStyle} title={tooltipLabel} aria-label={tooltipLabel}>
                <span aria-hidden="true">{symbol}</span>
                {detail ? <span style={{fontSize: 12}}>{detail}</span> : null}
            </span>
        </Tooltip>
    );
}

export function makeFlagLegend(items) {
    return (
        <div style={{display: "flex", gap: 12, flexWrap: "wrap", fontSize: 12, color: "var(--mantine-color-dimmed)", marginBottom: 8}}>
            {items.map((item) => (
                <span key={item.label} style={{display: "inline-flex", alignItems: "center", gap: 4}}>
                    <span aria-hidden="true">{item.symbol}</span>
                    {item.label}
                </span>
            ))}
        </div>
    );
}

export function makeTable(headers, rows) {
    return (
        <Table striped highlightOnHover withTableBorder withColumnBorders stickyHeader stickyHeaderOffset={0}>
            <Table.Thead>
                <Table.Tr>
                    {headers.map((header, headerIndex) => (
                        <Table.Th key={`header-${headerIndex}`}>{header}</Table.Th>
                    ))}
                </Table.Tr>
            </Table.Thead>
            <Table.Tbody>
                {rows.map((columns, rowIndex) => (
                    <Table.Tr key={`row-${rowIndex}`}>
                        {columns.map((column, colIndex) => (
                            <Table.Td key={`col-${rowIndex}-${colIndex}`}>{column ?? ""}</Table.Td>
                        ))}
                    </Table.Tr>
                ))}
            </Table.Tbody>
        </Table>
    );
}
