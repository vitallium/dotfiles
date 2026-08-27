// https://github.com/ZenkakuHiragana/my-chezmoi-config/blob/master/.chezmoitemplates/codex/extract-project-trust.js
"use strict";

const fs = require("node:fs");
const os = require("node:os");
const path = require("node:path");
const { TextDecoder } = require("node:util");

class ExtractError extends Error {}

function isHeader(line) {
  const stripped = line.trim();
  return stripped.startsWith("[") && stripped.endsWith("]");
}

function isProjectHeader(line) {
  const stripped = line.trim();
  return stripped === "[projects]" || stripped.startsWith("[projects.");
}

function extractProjectTrust(text) {
  const lines = text.split(/\r\n|[\n\r\v\f\x1c-\x1e\x85\u2028\u2029]/u);
  const blocks = [];
  let current = null;

  for (const line of lines) {
    if (isHeader(line)) {
      if (current !== null) {
        blocks.push(current.join("\n").trimEnd());
        current = null;
      }
      if (isProjectHeader(line)) current = [line];
      continue;
    }

    if (current !== null) current.push(line);
  }

  if (current !== null) blocks.push(current.join("\n").trimEnd());
  const output = blocks.filter((block) => block.trim()).join("\n\n");
  return output ? `${output}\n` : "";
}

function expandHomePath(filePath, home = os.homedir()) {
  if (filePath === "~") return home;
  if (filePath.startsWith("~/") || filePath.startsWith("~\\")) {
    return path.join(home, filePath.slice(2));
  }
  return filePath;
}

function readUtf8File(filePath) {
  const expandedPath = expandHomePath(filePath);
  let bytes;
  try {
    bytes = fs.readFileSync(expandedPath);
  } catch (error) {
    if (error.code === "ENOENT") return null;
    throw new ExtractError(
      `existing Codex config ${JSON.stringify(expandedPath)} could not be read: ${error.message}`,
    );
  }

  try {
    return new TextDecoder("utf-8", { fatal: true }).decode(bytes);
  } catch (error) {
    throw new ExtractError(
      `existing Codex config ${JSON.stringify(expandedPath)} is not valid UTF-8: ${error.message}`,
    );
  }
}

function main(argv) {
  if (argv.length !== 1) {
    process.stderr.write(
      "usage: extract-project-trust.js <existing-config-path>\n",
    );
    return 2;
  }

  try {
    const text = readUtf8File(argv[0]);
    if (text !== null) process.stdout.write(extractProjectTrust(text));
    return 0;
  } catch (error) {
    const detail = error instanceof ExtractError ? error.message : error.stack;
    process.stderr.write(`extract-project-trust.js: ${detail}\n`);
    return 1;
  }
}

module.exports = { expandHomePath, extractProjectTrust };

if (require.main === module) process.exitCode = main(process.argv.slice(2));
