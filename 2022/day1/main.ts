import {readFile} from '../lib/file';
import {maxL, sum} from '../lib/util';

function parse(data: string): Array<Array<number>> {
  const result: Array<Array<number>> = [[]];
  for (const line of data.split('\n')) {
    if (line === '') {
      result.push([]);
    } else {
      result[result.length - 1].push(parseInt(line));
    }
  }
  return result;
}

async function main1() {
  const fileData = await readFile('day1/input2.txt');
  const data = parse(fileData).map(sum);
  console.log(maxL(data));
}

main1();

async function main2() {
  const fileData = await readFile('day1/input2.txt');
  const data = parse(fileData).map(sum);
  data.sort((x, y) => x - y);
  console.log(sum(data.slice(data.length - 3)));
}

main2();
