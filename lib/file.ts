import * as fs from 'fs';
import {Readable} from 'stream';

export function readFile(path: string): Promise<string> {
  return new Promise((resolve, reject) => {
    fs.readFile(path, 'utf8', (err, data) => {
      if (err) {
        reject(err);
      } else {
        resolve(data);
      }
    });
  });
}

export function writeStream(path: string, stream: Readable): Promise<void> {
  const out = fs.createWriteStream(path);
  return new Promise((resolve: () => void) => {
    stream.on('data', chunk => {
      out.write(chunk);
    });
    stream.on('end', resolve);
  });
}
