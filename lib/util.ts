export function not<T>(f: (i: T) => boolean) {
  return (input: T) => !f(input);
}

export function isEmpty(i: string) {
  return i.length == 0;
}
