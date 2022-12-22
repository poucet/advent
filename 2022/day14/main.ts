import {readFile, writeStream} from '../lib/file';
import {maxL} from '../lib/util';
import {Canvas, CanvasRenderingContext2D} from 'canvas';

class Coordinate {
  constructor(public readonly x: number, public readonly y: number) {}
}

function parsePolyline(input: string): Array<Coordinate> {
  const result: Array<Coordinate> = [];
  for (const p of input.split(' -> ')) {
    const [x, y] = p.split(',');
    result.push(new Coordinate(Number(x), Number(y)));
  }
  return result;
}

function drawPolyline(
  polyline: Array<Coordinate>,
  ctx: CanvasRenderingContext2D
) {
  if (polyline.length < 2) return;
  ctx.beginPath();
  ctx.strokeStyle = 'rgb(255,255,255)';
  ctx.lineWidth = 1;
  ctx.moveTo(polyline[0].x + 0.5, polyline[0].y + 0.5);
  for (let i = 1; i < polyline.length; ++i) {
    ctx.lineTo(polyline[i].x + 0.5, polyline[i].y + 0.5);
  }
  ctx.stroke();
}

function dropSand(
  [x, y]: Array<number>,
  ctx: CanvasRenderingContext2D
): undefined | [number, number] {
  if (ctx.getImageData(x, y, 1, 1).data[0] !== 0) {
    return;
  }
  const width = ctx.canvas.width;
  const height = ctx.canvas.height;
  while (x >= 0 && x < width && y < height - 1) {
    if (ctx.getImageData(x, y + 1, 1, 1).data[0] === 0) {
      y += 1;
    } else if (ctx.getImageData(x - 1, y + 1, 1, 1).data[1] === 0) {
      x -= 1;
      y += 1;
    } else if (ctx.getImageData(x + 1, y + 1, 1, 1).data[1] === 0) {
      x += 1;
      y += 1;
    } else {
      return [x, y];
    }
  }
}

async function main1() {
  const input = await readFile('day14/input.txt');
  const segments = input
    .split('\n')
    .filter(l => l.length > 0)
    .map(parsePolyline);

  const width = maxL(segments.map(s => maxL(s.map(c => c.x))));
  const height = maxL(segments.map(s => maxL(s.map(c => c.y))));

  const canvas = new Canvas(width + 1, height + 3);
  const ctx = canvas.getContext('2d');
  ctx.fillStyle = 'rgb(0, 0, 0)';
  ctx.fillRect(0, 0, width + 1, height + 1);
  for (const segment of segments) {
    drawPolyline(segment, ctx);
  }

  let i = 0;
  for (i = 0; i < 10000; ++i) {
    const sandPos = dropSand([500, 0], ctx);
    if (!sandPos) break;
    ctx.fillStyle = 'rgb(255, 255, 0)';
    ctx.fillRect(sandPos[0], sandPos[1], 1, 1);
  }
  await writeStream('day14/input-1.png', canvas.createPNGStream());

  console.log(i);
}

async function main2() {
  const input = await readFile('day14/input.txt');
  const segments = input
    .split('\n')
    .filter(l => l.length > 0)
    .map(parsePolyline);

  const width = maxL(segments.map(s => maxL(s.map(c => c.x))));
  const height = maxL(segments.map(s => maxL(s.map(c => c.y))));

  const canvas = new Canvas(width * 2 + 1, height + 3);
  const ctx = canvas.getContext('2d');
  ctx.fillStyle = 'rgb(0, 0, 0)';
  ctx.fillRect(0, 0, 2 * width + 1, height + 1);
  drawPolyline(
    [new Coordinate(0, height + 2), new Coordinate(2 * width, height + 2)],
    ctx
  );
  for (const segment of segments) {
    drawPolyline(segment, ctx);
  }

  let i = 0;
  for (i = 0; i < 1000000; ++i) {
    const sandPos = dropSand([500, 0], ctx);
    if (!sandPos) break;
    ctx.fillStyle = 'rgb(255, 255, 0)';
    ctx.fillRect(sandPos[0], sandPos[1], 1, 1);
  }
  await writeStream('day14/input-2.png', canvas.createPNGStream());

  console.log(i);
}

async function main() {
  await main1();
  await main2();
}

main();
