import {readFile} from '../lib/file';

class Point {
  constructor(public x = 0, public y = 0) {}
  toString() {
    return `${this.x},${this.y}`;
  }

  save() {
    return new Point(this.x, this.y);
  }
}

class Knot extends Point {
  dx(knot: Knot) {
    return this.x - knot.x;
  }

  dy(knot: Knot) {
    return this.y - knot.y;
  }

  move(dir: String) {
    if (dir === 'L') {
      this.x -= 1;
    } else if (dir === 'R') {
      this.x += 1;
    } else if (dir === 'U') {
      this.y += 1;
    } else if (dir === 'D') {
      this.y -= 1;
    } else {
      throw new Error(`Unknown direction ${dir}`);
    }
  }
}

class Head extends Knot {}

class Tail extends Knot {
  public positions: Array<Point> = [];

  constructor() {
    super();
    this.positions.push(this.save());
  }

  approach(head: Head) {
    const dx = head.dx(this);
    const dy = head.dy(this);
    // Cpmpute X component
    if (Math.abs(dx) <= 1 && Math.abs(dy) <= 1) {
      // nothing
    } else if ((dx >= 1 && dy > 1) || (dy >= 1 && dx > 1)) {
      this.move('R');
      this.move('U');
    } else if ((dx >= 1 && dy < -1) || (dy <= -1 && dx > 1)) {
      this.move('R');
      this.move('D');
    } else if ((dx <= -1 && dy > 1) || (dx < -1 && dy >= 1)) {
      this.move('L');
      this.move('U');
    } else if ((dx <= -1 && dy < -1) || (dy <= -1 && dx < -1)) {
      this.move('L');
      this.move('D');
    } else if (dx < -1 && dy == 0) {
      this.move('L');
    } else if (dx > 1 && dy == 0) {
      this.move('R');
    } else if (dy > 1 && dx == 0) {
      this.move('U');
    } else if (dy < -1 && dx == 0) {
      this.move('D');
    } else {
      throw new Error(`Impossible combination ${dx} ${dy}`);
    }

    this.positions.push(this.save());
  }
}

function execute(input: Array<string>, numKnots: number) {
  const head = new Head();
  const tails = [...Array(numKnots).keys()].map(i => new Tail());

  for (const line of input) {
    if (line === '') continue;
    const [cmd, num] = line.split(' ');
    // console.log('----');
    // console.log(cmd, num);
    for (let i = 0; i < Number(num); ++i) {
      head.move(cmd);
      let leader = head;
      for (const tail of tails) {
        tail.approach(leader);
        leader = tail;
      }
      // console.log(tail.x, tail.y);
    }
  }

  return tails[tails.length - 1].positions;
}

async function main() {
  const data = await readFile('day9/input.txt');
  const pos = execute(data.split('\n'), 498);
  console.log(new Set(pos.map(p => p.toString())).size);
}

main();
