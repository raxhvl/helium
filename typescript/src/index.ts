import { promises as fs } from "fs";
import { Fixture, Transaction } from "./types/State";
import { ipsilon } from "./vm/evm";
import { parseHexToBytes } from "./types/Data";
import { validateEncoding } from "./lib/rlp";

const validate = (fixture: Fixture) => {
  // For each Fork key of post in the test, and for each of the elements of the list of
  for (const fork in fixture.post) {
    for (const post of fixture.post[fork]) {
      // 1. Using the indexes values construct the transaction object

      const tx: Transaction = {
        ...fixture.transaction,
        gasLimit: fixture.transaction.gasLimit[post.indexes.gas],
        data: fixture.transaction.data[post.indexes.data],
        value: fixture.transaction.value[post.indexes.value],
      };

      validateEncoding(post.txbytes, tx);

      const next_state = ipsilon(tx, fixture.pre);
    }
  }
};

// Main
(async () => {
  // Get the file path from the command line arguments
  const filePath = process.argv[2];
  const testName = process.argv[3];

  if (!filePath) {
    return console.error("Please provide a file path as an argument.");
  }

  const fixture: Fixture = parseHexToBytes(
    JSON.parse(await fs.readFile(filePath, "utf8"))[testName]
  );

  validate(fixture);
})();
