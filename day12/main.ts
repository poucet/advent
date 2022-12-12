import {readFile} from '../lib/file';

class Id {
  constructor(public x: number, public y: number) {}

  key() {
    return `${this.x},${this.y}`;
  }

  equals(other: Id) {
    return this.x === other.x && this.y === other.y;
  }
}

class AdjancencyMatrix<T extends {key: () => string}> {
  private readonly adjancency = new Map<string, Array<T>>();

  addEdge(from: T, to: T) {
    const key = from.key();
    if (!this.adjancency.has(key)) {
      this.adjancency.set(key, [to]);
    } else {
      this.adjancency.get(key)?.push(to);
    }
  }

  getNeighbors(from: T) {
    return this.adjancency.get(from.key()) ?? [];
  }
}

function canMove(from: string, to: string) {
  function normalize(char: string) {
    if (char === 'S') return 'a';
    if (char === 'E') return 'z';
    return char;
  }
  from = normalize(from);
  to = normalize(to);
  return from.charCodeAt(0) >= to.charCodeAt(0) - 1;
}

class Maze {
  constructor(
    public readonly starts: Array<Id>,
    public readonly end: Id,
    public readonly width: number,
    public readonly height: number,
    public readonly matrix: AdjancencyMatrix<Id>
  ) {}

  static parse(input: string, isStart: (char: string) => boolean) {
    const lines = input.split('\n');
    const startIds: Array<Id> = [];
    let endId: Id | undefined = undefined;
    const matrix = new AdjancencyMatrix<Id>();

    for (let j = 0; j < lines.length; ++j) {
      const previousLine = lines[j - 1];
      const nextLine = lines[j + 1];
      const currentLine = lines[j];
      for (let i = 0; i < currentLine.length; ++i) {
        const curChar = currentLine.charAt(i);
        const curId = new Id(i, j);
        if (isStart(curChar)) {
          startIds.push(curId);
        } else if (curChar === 'E') {
          endId = curId;
        }

        // Check Right
        if (i > 0 && canMove(curChar, currentLine[i - 1])) {
          matrix.addEdge(curId, new Id(i - 1, j));
        }
        // Check Left
        if (
          i < currentLine.length - 1 &&
          canMove(curChar, currentLine[i + 1])
        ) {
          matrix.addEdge(curId, new Id(i + 1, j));
        }
        // Check Top
        if (previousLine && canMove(curChar, previousLine.charAt(i))) {
          matrix.addEdge(curId, new Id(i, j - 1));
        }
        // Check Bottom
        if (nextLine && canMove(curChar, nextLine.charAt(i))) {
          matrix.addEdge(curId, new Id(i, j + 1));
        }
      }
    }
    if (startIds.length === 0 || !endId)
      throw new Error('Could not find start or end Ids');
    return new Maze(startIds, endId, lines[0].length, lines.length, matrix);
  }
}

function bfs(maze: Maze): number {
  const visited = new Map<string, number>();
  const queue = maze.starts;
  for (const s of queue) {
    visited.set(s.key(), 0);
  }

  while (queue.length > 0) {
    const current = queue.splice(0, 1)[0];
    const curDist = visited.get(current.key());
    if (curDist === undefined)
      throw new Error(
        `Impossible state, trying to get distance for ${current.key()}`
      );
    for (const n of maze.matrix.getNeighbors(current)) {
      if (visited.has(n.key())) continue;
      queue.push(n);
      visited.set(n.key(), curDist + 1);
    }
  }
  return visited.get(maze.end.key()) ?? -1;
}

async function main1() {
  const input = await readFile('day12/input.txt');
  const maze = Maze.parse(input, x => x === 'S');

  console.log(bfs(maze));
}

async function main2() {
  const input = await readFile('day12/input.txt');
  const maze = Maze.parse(input, x => x === 'S' || x === 'a');

  console.log(bfs(maze));
}

main1();
main2();
