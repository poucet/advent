import assert from 'assert';
import {readFile} from '../lib/file';
import {isEmpty, not} from '../lib/util';

class Stack {
  constructor(private crates: Array<string> = []) {}

  push(x: string) {
    this.crates.push(x);
  }
  pop(): string {
    const p = this.crates.pop();
    assert(p);
    return p;
  }
  concat(others: Array<string>) {
    for (let i = 0; i < others.length; ++i) {
      this.push(others[i]);
    }
  }
  slice(n: number): Array<string> {
    const idx = this.crates.length - n;
    const ret = this.crates.slice(idx);
    this.crates = this.crates.slice(0, idx);
    return ret;
  }

  toString() {
    return this.crates.join('');
  }
}

class Ship {
  constructor(public readonly stacks: Array<Stack> = []) {}

  static parseFrom(input: Array<string>) {
    const stackIndices = input[input.length - 1]
      .split(' ')
      .filter(not(isEmpty));
    const numShips = stackIndices.length;
    console.assert(numShips == Number(stackIndices[stackIndices.length - 1]));
    const ship = new Ship();
    for (const _idx of stackIndices) {
      ship.stacks.push(new Stack());
    }
    for (let l = input.length - 2; l >= 0; --l) {
      const row = [...input[l].matchAll(/(\[[A-Z][\]]|   )(?: |$)/g)];
      for (let i = 0; i < numShips; ++i) {
        const col = row[i][1];
        if (isEmpty(col.trim())) continue;
        else ship.stacks[i].push(col);
      }
    }
    return ship;
  }

  toString() {
    const result = [];
    for (let i = 0; i < this.stacks.length; ++i) {
      result.push(`${i + 1} ${this.stacks[i].toString()}`);
    }
    return result.join('\n');
  }
}

class Instruction {
  constructor(readonly n: number, readonly from: number, readonly to: number) {}

  // Format is: 'move <n> from <from> to <to>'
  static parseFrom(input: string) {
    const match = input.match(/move ([0-9]+) from ([0-9]+) to ([0-9]+)/);
    assert(match);
    return new Instruction(
      Number(match[1]),
      Number(match[2]) - 1,
      Number(match[3]) - 1
    );
  }

  apply(ship: Ship) {
    for (let i = 0; i < this.n; ++i) {
      ship.stacks[this.to].push(ship.stacks[this.from].pop());
    }
  }

  apply2(ship: Ship) {
    ship.stacks[this.to].concat(ship.stacks[this.from].slice(this.n));
  }

  toString() {
    return `move ${this.n} from ${this.from + 1} to ${this.to + 1}`;
  }
}

async function main() {
  const fileData = await readFile('day5/input.txt');
  const lines = fileData.split('\n');
  const splitIndex = lines.findIndex(isEmpty);
  const ship = Ship.parseFrom(lines.slice(0, splitIndex));
  const instructions = lines
    .slice(splitIndex + 1)
    .filter(not(isEmpty))
    .map(Instruction.parseFrom);
  for (const i of instructions) {
    console.log(ship.toString());
    console.log(i.toString());
    i.apply2(ship);
    console.log('---');
  }
  console.log(ship.toString());
}

main();
