import {readFile} from '../lib/file';

enum PIECE_TYPE {
  HORIZONTAL_LINE = 1,
  PLUS = 2,
  RIGHT_ANGLE = 3,
  VERTICAL_LINE = 4,
  SQUARE = 5,
}

const PIECES = new Map<PIECE_TYPE, string>([
  [PIECE_TYPE.HORIZONTAL_LINE, '####'],
  [
    PIECE_TYPE.PLUS,
    `.@.
     @@@
     .@.`,
  ],
  [
    PIECE_TYPE.RIGHT_ANGLE,
    `..@
     ..@
     @@@`,
  ],
  [
    PIECE_TYPE.VERTICAL_LINE,
    `@
     @
     @
     @`,
  ],
  [
    PIECE_TYPE.SQUARE,
    `@@
     @@`,
  ],
]);

type Row = string;
function emptyRow(width: number): Row {
  return '.'.repeat(width);
}

type Grid = Array<Row>;

function padRight(row: Row, width: number) {
  return row.concat('.'.repeat(width - row.length));
}

function mergeRows(a: Row, b: Row): Row {
  if (a.length !== b.length)
    throw new Error(`Rows (${a}) and (${b}) do not match in length`);
  const result: Array<string> = [];
  for (let i = 0; i < a.length; ++i) {
    if (a.charAt(i) === '.' && b.charAt(i) === '.') {
      result.push('.');
    } else {
      result.push('#');
    }
  }
  return result.join('');
}

function intersects(a: Row | undefined, b: Row | undefined) {
  if (!a || !b) return false;
  if (a.length !== b.length)
    throw new Error(`Rows (${a}) and (${b}) do not match in length`);
  for (let i = 0; i < a.length; ++i) {
    if (a.charAt(i) !== '.' && b.charAt(i) !== '.') {
      return true;
    }
  }
  return false;
}

class Piece {
  constructor(
    // Height of bottom line
    public y: number,
    public width: number,
    public grid: Grid
  ) {}

  get height() {
    return this.y + this.grid.length;
  }

  right(): Piece {
    for (const row of this.grid) {
      if (row.charAt(this.width - 1) !== '.') return this;
    }
    for (let i = 0; i < this.grid.length; ++i) {
      this.grid[i] = '.'.concat(this.grid[i].slice(0, this.width - 1));
    }
    return this;
  }

  left(): Piece {
    for (const row of this.grid) {
      if (row.charAt(0) !== '.') return this;
    }
    for (let i = 0; i < this.grid.length; ++i) {
      this.grid[i] = this.grid[i].slice(1).concat('.');
    }
    return this;
  }

  down(): Piece {
    this.y -= 1;
    return this;
  }

  static parse(x: number, y: number, width: number, repr: String): Piece {
    const rows = repr.split('\n');
    const piece = new Piece(
      y,
      width,
      rows.reverse().map(x => padRight(x.trim(), width))
    );
    for (let i = 0; i < x; ++i) {
      piece.right();
    }
    return piece;
  }

  clone(): Piece {
    return new Piece(this.y, this.width, [...this.grid]);
  }

  toString() {
    return [...this.grid].reverse().join('\n');
  }
}

class Chamber {
  private grid: Array<Row>;
  private offset = 0;

  constructor(readonly width: number) {
    this.grid = new Array<Row>('-'.repeat(width));
  }

  get height() {
    // Figure out the actual height that's free.
    return this.grid.length + this.offset;
  }

  newPiece(type: PIECE_TYPE): Piece {
    return Piece.parse(2, this.height + 3, this.width, PIECES.get(type) ?? '');
  }

  toString() {
    return [...this.grid].reverse().join('\n');
  }

  intersects(piece: Piece): boolean {
    for (let i = piece.y; i < piece.height; ++i) {
      if (intersects(this.grid[i], piece.grid[i - piece.y])) return true;
    }
    return false;
  }

  merge(piece: Piece) {
    while (piece.height > this.height) {
      this.grid.push(emptyRow(this.width));
    }
    for (let i = piece.y; i < piece.height; ++i) {
      this.grid[i] = mergeRows(this.grid[i], piece.grid[i - piece.y]);
    }
    return this;
  }

  drop(piece: Piece, command: string): boolean {
    switch (command) {
      case '<':
        if (!this.intersects(piece.clone().left())) {
          piece.left();
        }
        break;
      case '>':
        if (!this.intersects(piece.clone().right())) {
          piece.right();
        }
        break;
      default:
        throw new Error(`Unknown command: (${command})`);
    }
    // Reset the piece of it intersects

    if (this.intersects(piece.clone().down())) {
      this.merge(piece);
      return false;
    } else {
      piece.down();
      return true;
    }
  }

  clone(): Chamber {
    const chamber = new Chamber(this.width);
    chamber.grid = [...this.grid];
    return chamber;
  }
}

function dropNRocks(input: string, numRocks: number) {
  const pieceOrder = [
    PIECE_TYPE.HORIZONTAL_LINE,
    PIECE_TYPE.PLUS,
    PIECE_TYPE.RIGHT_ANGLE,
    PIECE_TYPE.VERTICAL_LINE,
    PIECE_TYPE.SQUARE,
  ];
  const chamber = new Chamber(7);
  let j = 0;
  for (let i = 0; i < numRocks; ++i) {
    const piece = chamber.newPiece(pieceOrder[i % pieceOrder.length]);
    while (chamber.drop(piece, input.charAt(j % input.length))) {
      ++j;
    }
    ++j;
  }
  return [j, chamber.height - 1];
}

async function problem1(path: string) {
  const input = (await readFile(path)).trim();
  console.log(dropNRocks(input, 2022)[1]);
}

function dropNTimes(input: string, factor: number) {
  const pieceOrder = [
    PIECE_TYPE.HORIZONTAL_LINE,
    PIECE_TYPE.PLUS,
    PIECE_TYPE.RIGHT_ANGLE,
    PIECE_TYPE.VERTICAL_LINE,
    PIECE_TYPE.SQUARE,
  ];
  const chamber = new Chamber(7);
  let j = 0;
  let i = 0;
  while (j < input.length * factor) {
    const piece = chamber.newPiece(pieceOrder[i % pieceOrder.length]);
    while (chamber.drop(piece, input.charAt(j % input.length))) {
      ++j;
    }
    ++j;
    ++i;
  }
  return [i, chamber.height - 1];
}

async function problem2(path: string) {
  const input = await (await readFile(path)).trim();
  const [i1, h1] = dropNTimes(input, 100);
  const [i2, h2] = dropNTimes(input, 200);
  const dh = h2 - h1;
  const di = i2 - i1;
  console.log(
    dh * Math.floor(1000000000000 / di) +
      dropNRocks(input, 1000000000000 % di)[1]
  );
}

async function main() {
  await problem1('day17/input.txt');
  await problem2('day17/input.txt');
}

main();
