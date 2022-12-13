import {readFile} from '../lib/file';
import {not, isEmpty} from '../lib/util';

class Interval {
  constructor(readonly start: number, readonly end: number) {}

  static parseFrom(input: string) {
    const [a, b] = input.split('-');
    return new Interval(Number(a), Number(b));
  }

  contains(other: Interval) {
    return other.start >= this.start && other.end <= this.end;
  }

  overlaps(other: Interval) {
    return other.end >= this.start && this.end >= other.start;
  }

  toString() {
    return `${this.start}-${this.end}`;
  }
}

class Pair<T> {
  constructor(readonly a: T, readonly b: T) {}

  static parseFrom<T>(input: string, parse: (i: string) => T) {
    const [a, b] = input.split(',');
    return new Pair<T>(parse(a), parse(b));
  }
  toString() {
    return `${this.a},${this.b}`;
  }
}

function parse(line: string): Pair<Interval> {
  return Pair.parseFrom(line, Interval.parseFrom);
}

async function main() {
  const fileData = await readFile('day4/input2.txt');
  const data = fileData.split('\n').filter(not(isEmpty)).map(parse);
  const containing = data.filter(x => x.a.contains(x.b) || x.b.contains(x.a));
  const overlapping = data.filter(x => x.a.overlaps(x.b));
  console.log(data.length);
  console.log(containing.length);
  console.log(overlapping.length);
}

main();
