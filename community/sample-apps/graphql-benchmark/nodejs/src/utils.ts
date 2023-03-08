export function keyByArray<T>(input: T[], id: keyof T) {
    return input.reduce((acc, result) => {
        const existing = acc[result[id] as string | number];
        acc[result[id as keyof T] as string | number] = existing
            ? [...existing, result]
            : [result];
        return acc;
    }, {} as Record<string | number, T[]>);
}
