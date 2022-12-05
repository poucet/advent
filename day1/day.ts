import {readFile} from '../lib/file';

function parse(data: string): Array<Array<number>> {
  const result: Array<Array<number>> = [[]];
  for (const line of data.split('\n')) {
    if (line == '') {
      result.push([]);
    } else {
      result[result.length - 1].push(parseInt(line));
    }
  }
  return result;
}

function sum(numbers: Array<number>): number {
  if (numbers.length === 0) return 0;
  if (numbers.length === 1) return numbers[0];
  return numbers.reduce((p, c) => p + c);
}

function maxL(numbers: Array<number>) {
  if (numbers.length === 0) return 0;
  if (numbers.length === 1) return numbers[0];
  return numbers.reduce((p, c) => Math.max(p, c));
}

async function main() {
  const fileData = await readFile('day1/input.txt');
  const data = parse(fileData).map(sum);
  data.sort();
  console.log(sum(data.slice(data.length - 3)));
}

main();
