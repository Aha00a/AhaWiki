import React from "react";
import {Table} from "@mantine/core";

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

export function makeTable(headers, rows) {
    return (
        <Table striped highlightOnHover withTableBorder withColumnBorders stickyHeader stickyHeaderOffset={0}>
            <Table.Thead>
                <Table.Tr>
                    {headers.map((header) => (
                        <Table.Th key={header}>{header}</Table.Th>
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
