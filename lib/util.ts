export function not<T>(f: (i: T) => boolean) {
  return (input: T) => !f(input);
}

export function isEmpty(i: string) {
  return i.length === 0;
}

export function sum(numbers: Array<number>): number {
  if (numbers.length === 0) return 0;
  if (numbers.length === 1) return numbers[0];
  return numbers.reduce((p, c) => p + c);
}
