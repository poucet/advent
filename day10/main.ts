import {readFile} from '../lib/file';

class CPU {
  private x = 1;
  private t = 1;
  constructor(
    private readonly callback: (tick: number, value: number) => void
  ) {}

  execute(instruction: string) {
    if (instruction === 'noop') {
      this.noop();
    } else if (instruction.startsWith('addx ')) {
      const [_cmd, num] = instruction.split(' ');
      this.addx(Number(num));
    } else {
      throw new Error(`Unknown instruction: ${instruction}`);
    }
  }

  noop() {
    this.tick();
  }

  addx(n: number) {
    this.tick();
    this.x += n;
    this.tick();
  }

  tick() {
    this.t += 1;
    this.callback(this.t, this.x);
  }
}

async function main() {
  const input = await readFile('day10/input.txt');
  let s = 0;
  const cpu = new CPU((t, v) => {
    if ((t - 20) % 40 === 0 && t <= 220) {
      s += t * v;
    }
  });
  for (const line of input.split('\n')) {
    if (line === '') continue;
    cpu.execute(line);
  }
  console.log(s);
}

async function main2() {
  const input = await readFile('day10/input.txt');
  let line: Array<string> = ['#'];
  const cpu = new CPU((t, v) => {
    if (Math.abs(v - ((t - 1) % 40)) <= 1) {
      line.push('#');
    } else {
      line.push('.');
    }
    if (t % 40 === 0) {
      console.log(line.join(''));
      line = [];
    }
  });
  for (const line of input.split('\n')) {
    if (line === '') continue;
    cpu.execute(line);
  }
}

main2();
