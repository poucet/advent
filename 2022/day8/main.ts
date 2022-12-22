import {readFile} from '../lib/file';

class Matrix {
  // Data is organized in columns.
  private data: Array<Array<number>> = [];
  constructor() {}

  get numRows() {
    return this.data.length;
  }

  get numColumns() {
    if (this.numRows === 0) return 0;
    return this.data[0].length;
  }

  row(i: number): Array<number> {
    if (i >= this.numRows) {
      return [];
    }
    return [...this.data[i]];
  }

  column(i: number): Array<number> {
    if (i >= this.numColumns) {
      return [];
    }
    const result = [];
    for (const row of this.data) {
      result.push(row[i]);
    }
    return result;
  }

  static parseFrom(input: string) {
    const matrix = new Matrix();
    const lines = input.split('\n');
    for (const line of lines) {
      const row: Array<number> = [];
      if (line === '') continue;
      for (const c of line.split('')) {
        row.push(Number(c));
      }
      matrix.data.push(row);
    }
    return matrix;
  }
}

function traverse(numbers: Array<number>, is: (i: number, v: number) => void) {
  let min = -1;
  for (let i = 0; i < numbers.length; ++i) {
    if (numbers[i] > min) {
      is(i, numbers[i]);
      min = numbers[i];
    }
  }
}

function visible(
  matrix: Matrix,
  is: (i: number, j: number, v: number) => void
) {
  for (let r = 0; r < matrix.numRows; ++r) {
    const row = matrix.row(r);
    traverse(row, (c: number, v: number) => is(r, c, v));
    row.reverse();
    traverse(row, (c: number, v: number) => is(r, row.length - 1 - c, v));
  }
  for (let c = 0; c < matrix.numColumns; ++c) {
    const column = matrix.column(c);
    traverse(column, (r: number, v: number) => is(r, c, v));
    column.reverse();
    traverse(column, (r: number, v: number) => is(column.length - 1 - r, c, v));
  }
}

function view(numbers: Array<number>, pos: number): number {
  const height = numbers[pos];
  let s = 0;
  for (let i = pos + 1; i < numbers.length; ++i) {
    s += 1;
    if (numbers[i] >= height) break;
  }
  return s;
}

function scenicScore(matrix: Matrix, x: number, y: number) {
  const row = matrix.row(x);
  const visR = view(row, y);
  row.reverse();
  const visL = view(row, row.length - 1 - y);
  const column = matrix.column(y);
  const visD = view(column, x);
  column.reverse();
  const visU = view(column, column.length - 1 - x);
  return visR * visL * visD * visU;
}

async function main1() {
  const data = await readFile('day8/input2.txt');
  const matrix = Matrix.parseFrom(data);
  const isVisible = new Set();
  visible(matrix, (r: number, c: number, _v: number) => {
    isVisible.add(`${r}-${c}`);
  });
  console.log(isVisible.size);
}

async function main2() {
  const data = await readFile('day8/input2.txt');
  const matrix = Matrix.parseFrom(data);
  const isVisible = new Set();
  let maxScore = -1;
  for (let x = 0; x < matrix.numRows; ++x) {
    for (let y = 0; y < matrix.numColumns; ++y) {
      maxScore = Math.max(maxScore, scenicScore(matrix, x, y));
    }
  }
  console.log(maxScore);
}

main1();
main2();
