import * as cosmo from '../../target/scala-3.3.3/cosmo-opt/main.js';
import { readFileSync, writeFileSync } from 'fs';

const input  = process.argv[2];
const output = process.argv[3];

const inputData = readFileSync(input, 'utf8');
const outputData = cosmo.Cosmo.compile(inputData);

writeFileSync(output, outputData, 'utf8');
