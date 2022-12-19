import {readFile} from '../lib/file';
import {sum, maxL} from '../lib/util';

type Point = [number, number, number];

function neighbors(point: Point): Array<Point> {
  return [
    [point[0] - 1, point[1], point[2]],
    [point[0] + 1, point[1], point[2]],
    [point[0], point[1] - 1, point[2]],
    [point[0], point[1] + 1, point[2]],
    [point[0], point[1], point[2] - 1],
    [point[0], point[1], point[2] + 1],
  ];
}

class Cube {
  constructor(public readonly point: Point) {}

  get i() {
    return this.point[0];
  }

  get j() {
    return this.point[1];
  }

  get k() {
    return this.point[2];
  }

  static parse(input: string) {
    const [x, y, z] = input.split(',');
    return new Cube([Number(x), Number(y), Number(z)]);
  }

  sides(): Array<Point> {
    return neighbors(this.point);
  }

  toString() {
    return `${this.i},${this.j},${this.k}`;
  }
}

type Steam = true;
type Matrix1D = Array<Cube | Steam>;
type Matrix2D = Array<Matrix1D>;
type Matrix3D = Array<Matrix2D>;

class Volcano {
  private readonly matrix: Matrix3D = new Array<Matrix2D>();
  constructor() {}

  getAt(point: Point): Steam | Cube | undefined {
    const plane = this.matrix[point[0]];
    if (plane === undefined) return undefined;
    const row = plane[point[1]];
    if (row === undefined) return undefined;
    return row[point[2]];
  }

  setAt(point: Point, value: Cube | Steam) {
    let plane = this.matrix[point[0]];
    if (plane === undefined) {
      plane = this.matrix[point[0]] = [];
    }
    let row = plane[point[1]];
    if (row === undefined) {
      row = plane[point[1]] = [];
    }
    if (row[point[2]] !== undefined)
      throw new Error(
        `Trying to overwrite occupied space ${point} with ${value.toString()}`
      );
    row[point[2]] = value;
  }

  addCube(cube: Cube) {
    this.setAt(cube.point, cube);
  }

  addSteam(point: Point) {
    this.setAt(point, true);
  }

  flood(start: Point) {
    const maxI = this.matrix.length;
    const maxJ = maxL(this.matrix.map(p => p.length));
    const maxK = maxL(this.matrix.map(p => maxL(p.map(r => r.length))));

    let points = [start];
    while (points.length > 0) {
      const point = points.pop() as Point;
      if (point[0] < -1 || point[0] > maxI) continue;
      if (point[1] < -1 || point[1] > maxJ) continue;
      if (point[2] < -1 || point[2] > maxK) continue;
      if (this.getAt(point) !== undefined) continue;
      this.addSteam(point);
      points = this.adjancentFree(point).concat(points);
    }
  }

  adjancentFree(point: Point): Array<Point> {
    return neighbors(point).filter(p => this.getAt(p) === undefined);
  }

  adjancentSteam(point: Point): Array<Point> {
    return neighbors(point).filter(p => this.getAt(p) === true);
  }
}

async function problem1(path: string) {
  const input = await readFile(path);
  const cubes = input
    .split('\n')
    .filter(l => l !== '')
    .map(l => Cube.parse(l));
  const volcano = new Volcano();
  for (const cube of cubes) {
    volcano.addCube(cube);
  }
  console.log(sum(cubes.map(c => volcano.adjancentFree(c.point).length)));
}

async function problem2(path: string) {
  const input = await readFile(path);
  const cubes = input
    .split('\n')
    .filter(l => l !== '')
    .map(l => Cube.parse(l));
  const volcano = new Volcano();
  for (const cube of cubes) {
    volcano.addCube(cube);
  }
  volcano.flood([0, 0, 0]);
  console.log(sum(cubes.map(c => volcano.adjancentSteam(c.point).length)));
}

async function main() {
  await problem1('day18/input.txt');
  await problem2('day18/input.txt');
}

main();
