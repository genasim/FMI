const { Transform } = require("node:stream");
const fs = require("node:fs");

const dictionary = require("./dictionary.js");

function createFileReadStream(pathToFile) {
  const fileStream = fs.createReadStream(pathToFile, { encoding: "utf-8" });

  let buffer = "";
  fileStream.on("data", (chunk) => (buffer += chunk));
  return fileStream;
}

const censorFileStream = new Transform({
  highWaterMark: 20,
  encoding: "utf-8",
  transform(data, _, callback) {
    const text = data.toString();
    const words = text.split(" ");k

    const censoredWords = words.map((word) => dictionary[word] ?? word);
    const censoredText = censoredWords.join(" ");

    this.push(censoredText);
    callback();
  },
});

const createWriteStream = (pathToSave) => fs.createWriteStream(pathToSave);

const censorFile = (srcPath, destPath) => {
  createFileReadStream(srcPath)
    .pipe(censorFileStream)
    .pipe(createWriteStream(destPath))
    .on("finish", () => {
      console.log(`File at ${srcPath} has been successfully censored and saved at ${destPath}
  `);
    });
};

const pathToFile = __dirname + "/files/bad-text.txt";
const pathToSave = __dirname + "/files/good-text.txt";
censorFile(pathToFile, pathToSave);
