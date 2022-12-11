import {inspect} from 'util';
import {readFile} from '../lib/file';

interface Expression {
  evaluate(old: number): number;
}

class Constant implements Expression {
  constructor(private value: number) {}
  evaluate(_: number) {
    return this.value;
  }
}

class Variable implements Expression {
  constructor() {}
  evaluate(old: number) {
    return old;
  }
}

enum Operator {
  PLUS = 1,
  MULT = 2,
}

class BinaryExpression implements Expression {
  constructor(
    private left: Expression,
    private right: Expression,
    private binop: Operator
  ) {}

  evaluate(old: number) {
    const l = this.left.evaluate(old);
    const r = this.right.evaluate(old);
    switch (this.binop) {
      case Operator.PLUS:
        return l + r;
      case Operator.MULT:
        return l * r;
    }
  }
}

function parseOperator(input: string): Operator {
  if (input === '+') return Operator.PLUS;
  if (input === '*') return Operator.MULT;
  throw new Error(`Invalid operator: ${input}`);
}

function parseExpression(input: string): Expression {
  if (input === 'old') return new Variable();
  if (input.match(/^[0-9]+$/)) return new Constant(Number(input));
  const parts = input.split(' ');
  if (parts.length !== 3) {
    throw new Error(`Can not parse: ${input}`);
  }

  const l = parseExpression(parts[0]);
  const r = parseExpression(parts[2]);
  const op = parseOperator(parts[1]);
  return new BinaryExpression(l, r, op);
}

// Maps a number to a monkey id
class Condition {
  constructor(
    public readonly div: number,
    private readonly trueMonkey: number,
    private readonly falseMonkey: number
  ) {}

  evaluate(value: number): number {
    return value % this.div === 0 ? this.trueMonkey : this.falseMonkey;
  }

  static parse(input: Array<string>): Condition {
    /*
      Test: divisible by 23
        If true: throw to monkey 2
        If false: throw to monkey 3
    */
    const matchCond = input[0].match(/ *Test: divisible by ([0-9]+)$/);
    if (!matchCond) throw new Error(`Could not parse Test: ${input[0]}`);
    const div = Number(matchCond[1]);
    const matchTrue = input[1].match(/ *If true: throw to monkey ([0-9]+)/);
    if (!matchTrue)
      throw new Error(`Could not parse true condition: ${input[1]}`);
    const trueMonkey = Number(matchTrue[1]);
    const matchFalse = input[2].match(/ *If false: throw to monkey ([0-9]+)/);
    if (!matchFalse)
      throw new Error(`Could not parse true condition: ${input[2]}`);
    const falseMonkey = Number(matchFalse[1]);
    return new Condition(div, trueMonkey, falseMonkey);
  }
}

class Monkey {
  public ticks = 0;
  constructor(
    private items: Array<number>,
    public readonly expression: Expression,
    public readonly condition: Condition
  ) {}

  throwTo(worry: number) {
    this.items.push(worry);
  }

  static parseStartingItems(input: string): Array<number> {
    const match = input.match(/ *Starting items: ((?:[0-9]+(?:, |$))*)$/);
    if (!match) return [];

    return [...match[1].split(', ')].map(v => Number(v));
  }

  static parseOperation(input: string): Expression {
    const match = input.match(/ *Operation: new = (.*)$/);
    if (!match) throw new Error(`Could not parse operation: ${input}`);

    return parseExpression(match[1]);
  }

  static parse(input: Array<string>): Monkey {
    /*
    Monkey 0:
      Starting items: 79, 98
      Operation: new = old * 19
      Test: divisible by 23
        If true: throw to monkey 2
        If false: throw to monkey 3
    */

    return new Monkey(
      Monkey.parseStartingItems(input[1]),
      Monkey.parseOperation(input[2]),
      Condition.parse(input.slice(3))
    );
  }

  do(monkeys: Array<Monkey>, reducer: (worry: number) => number) {
    const items = this.items;
    this.items = [];
    for (const item of items) {
      const worry = reducer(this.inspect(item));
      monkeys[this.condition.evaluate(worry)].throwTo(worry);
    }
  }

  inspect(item: number) {
    this.ticks += 1;
    item = this.expression.evaluate(item);
    // item = Math.floor(item / 3);
    return item;
  }

  print() {
    console.log(this.items);
  }
}
async function main() {
  const data = await readFile('day11/input.txt');
  const lines = data.split('\n');
  const monkeys = [];
  while (lines.length > 0) {
    while (lines.length > 0 && !lines[0].match(/Monkey [0-9]:/))
      lines.splice(0, 1);
    if (lines.length === 0) break;
    monkeys.push(Monkey.parse(lines.splice(0, 6)));
  }

  for (let i = 0; i < 20; i++) {
    for (const monkey of monkeys) {
      monkey.do(monkeys, v => Math.floor(v / 3));
    }
  }
  const ticks = monkeys.map(m => m.ticks);
  ticks.sort((a: number, b: number) => a - b);
  console.log(ticks);
}

async function main2() {
  const data = await readFile('day11/input.txt');
  const lines = data.split('\n');
  const monkeys = [];
  while (lines.length > 0) {
    while (lines.length > 0 && !lines[0].match(/Monkey [0-9]:/))
      lines.splice(0, 1);
    if (lines.length === 0) break;
    monkeys.push(Monkey.parse(lines.splice(0, 6)));
  }
  const lcm = monkeys
    .map(m => m.condition.div)
    .reduce((a: number, b: number) => a * b);
  for (let i = 0; i < 10000; i++) {
    for (const monkey of monkeys) {
      monkey.do(monkeys, v => v ^ lcm);
    }
  }
  const ticks = monkeys.map(m => m.ticks);
  ticks.sort((a: number, b: number) => a - b);
  console.log(ticks);
}

main();
main2();
