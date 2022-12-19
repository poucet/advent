import {readFile} from '../lib/file';
import {sum, maxL} from '../lib/util';

class Valve {
  constructor(
    public readonly name: string,
    public readonly rate: number,
    public readonly neighbors: Array<string>
  ) {}

  static parse(input: string): Valve {
    const match = input.match(
      /Valve ([A-Z][A-Z]) has flow rate=([0-9]+); tunnels? leads? to valves? ((?:[A-Z][A-Z](?:, )?)*)/
    );
    if (!match) throw new Error(`Could not parse: ${input}`);
    return new Valve(match[1], Number(match[2]), match[3].split(', '));
  }
}

function solve1(valves: Map<string, Valve>, maxMinutes: number): number {
  const valve = valves.get('AA');
  if (!valve) throw new Error('Could not find valve AA');
  function traverse(
    current: Valve,
    comingFrom: Valve,
    open: Array<Valve>,
    total: number,
    minutes: number
  ): number {
    if (minutes <= 0) return total;
    const airflow = sum(open.map(v => v.rate));
    if (open.length === valves.size) return airflow * minutes;

    if (minutes > 1 && current.rate > 0 && !open.includes(current)) {
      return maxL(
        current.neighbors.map((name: string) => {
          const neighbor = valves.get(name);
          if (!neighbor) throw new Error(`Could not find valve ${name}`);
          return traverse(
            neighbor,
            current,
            open.concat([current]),
            total + airflow + airflow + current.rate,
            minutes - 2
          );
        })
      );
    } else {
      return maxL(
        current.neighbors
          .filter(name => name !== comingFrom.name)
          .map((name: string) => {
            const neighbor = valves.get(name);
            if (!neighbor) throw new Error(`Could not find valve ${name}`);
            return traverse(
              neighbor,
              current,
              open,
              total + airflow,
              minutes - 1
            );
          })
      );
    }
  }
  return traverse(valve, valve, [], 0, maxMinutes);
}

function solve2(valves: Map<string, Valve>, maxMinutes: number): number {
  return 0;
}

async function main1(path: string) {
  const input = await readFile(path);
  const valves = input
    .split('\n')
    .filter(l => l !== '')
    .map(l => Valve.parse(l));
  const keyedValves = new Map<string, Valve>(valves.map(v => [v.name, v]));
  console.log(solve1(keyedValves, 30));
}

async function main2(path: string) {
  const input = await readFile(path);
  const valves = input
    .split('\n')
    .filter(l => l !== '')
    .map(l => Valve.parse(l));
  const keyedValves = new Map<string, Valve>(valves.map(v => [v.name, v]));
  console.log(solve2(keyedValves, 26));
}

async function main() {
  await main1('day16/input2.txt');
  await main2('day16/input2.txt');
}

main();
