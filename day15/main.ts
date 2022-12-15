import {readFile} from '../lib/file';
import {sum} from '../lib/util';

type Position = [number, number];

function hammingDistance([xa, ya]: Position, [xb, yb]: Position) {
  return Math.abs(xb - xa) + Math.abs(yb - ya);
}

type Range = [number, number];

function intersects(a: Range, b: Range): boolean {
  return a[0] <= b[1] && b[0] <= a[1];
}

function adjacent(a: Range, b: Range): boolean {
  return a[0] === b[1] + 1 || a[1] + 1 === b[0];
}

function merge(a: Range, b: Range): Range {
  return [Math.min(a[0], b[0]), Math.max(a[1], b[1])];
}
function rangeSize(r: Range) {
  return r[1] - r[0] + 1;
}

function mergeRanges(ranges: Array<Range>) {
  if (ranges.length <= 1) return ranges;
  ranges.sort((a, b) => a[0] - b[0]);
  const result = [ranges[0]];
  for (let i = 1; i < ranges.length; ++i) {
    const range = ranges[i];
    const tail = result[result.length - 1];
    if (intersects(range, tail) || adjacent(range, tail)) {
      const merged = merge(range, tail);
      tail[0] = merged[0];
      tail[1] = merged[1];
    } else {
      result.push(range);
    }
  }
  return result;
}

function intersectRange(a: Range, b: Range) {
  if (!intersects(a, b)) return undefined;
  return [Math.max(a[0], b[0]), Math.min(a[1], b[1])];
}

function intersectRanges(a: Range, ranges: Array<Range>) {
  return ranges
    .map(r => intersectRange(a, r))
    .filter(x => x !== undefined) as Array<Range>;
}

class Sensor {
  public readonly range: number;
  constructor(
    private readonly self: Position,
    private readonly beacon: Position
  ) {
    this.range = hammingDistance(this.self, this.beacon);
  }

  get x() {
    return this.self[0];
  }

  get y() {
    return this.self[1];
  }

  get bx() {
    return this.beacon[0];
  }

  get by() {
    return this.beacon[1];
  }

  static parse(input: string): Sensor | undefined {
    const match = input.match(
      /^Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)$/
    );
    if (!match) return undefined;
    return new Sensor(
      [Number(match[1]), Number(match[2])],
      [Number(match[3]), Number(match[4])]
    );
  }

  toString(): string {
    return `${this.self[0]},${this.self[1]} -> ${this.beacon[0]},${this.beacon[1]}`;
  }

  project(y: number): Range | undefined {
    if (Math.abs(this.self[1] - y) > this.range) return undefined;
    return [
      this.x - (this.range - Math.abs(this.y - y)),
      this.x + (this.range - Math.abs(this.y - y)),
    ];
  }
}

async function main1(path: string, y: number) {
  const input = await readFile(path);
  const sensors: Array<Sensor> = [];
  for (const l of input.split('\n')) {
    const sensor = Sensor.parse(l);
    if (sensor) sensors.push(sensor);
  }

  const ranges = mergeRanges(
    sensors.map(s => s.project(y)).filter(x => x !== undefined) as Array<Range>
  );
  const beaconRanges = mergeRanges(
    sensors
      .map(s => (s.by === y ? [s.bx, s.bx] : undefined))
      .filter(x => x !== undefined) as Array<Range>
  );
  const totalRange = sum(ranges.map(rangeSize));
  const beaconsOnLine = sum(beaconRanges.map(rangeSize));

  console.log(totalRange - beaconsOnLine);
}

async function main2(path: string, max: number) {
  const input = await readFile(path);
  const sensors: Array<Sensor> = [];
  for (const l of input.split('\n')) {
    const sensor = Sensor.parse(l);
    if (sensor) sensors.push(sensor);
  }
  for (let i = 0; i < max; ++i) {
    const ranges = mergeRanges(
      intersectRanges(
        [0, max],
        sensors
          .map(s => s.project(i))
          .filter(x => x !== undefined) as Array<Range>
      )
    );
    if (ranges.length === 1) continue;
    console.log(i + max * (ranges[0][1] + 1));
    console.log(i);
    console.log(ranges);
  }
}

async function main() {
  await main1('day15/input.txt', 2000000);
  await main2('day15/input.txt', 4000000);
}

main();
