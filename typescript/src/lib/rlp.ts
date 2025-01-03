import rlp from "rlp";
import { Transaction } from "../types/State";

export function rlpToTransaction(txbytes: Buffer): Transaction {
  const decodedTx = rlp.decode(txbytes);
  return {
    //@ts-ignore
    nonce: Buffer.from(decodedTx[0].length ? decodedTx[0] : [0]),
    //@ts-ignore
    gasPrice: Buffer.from(decodedTx[1]),
    //@ts-ignore
    gasLimit: Buffer.from(decodedTx[2]),
    //@ts-ignore
    to: Buffer.from(decodedTx[3]),
    //@ts-ignore
    value: Buffer.from(decodedTx[4]),
    //@ts-ignore
    data: Buffer.from(decodedTx[5]),
    //@ts-ignore
    v: Buffer.from(decodedTx[6]),
    //@ts-ignore
    r: Buffer.from(decodedTx[7]),
    //@ts-ignore
    s: Buffer.from(decodedTx[8]),
  };
}

export function validateEncoding(encoding: Buffer, tx: Transaction) {
  const decodedTransaction = rlpToTransaction(encoding);

  // Get the keys of the tx object
  const keys = ["nonce", "gasPrice", "gasLimit", "data", "value", "to"];

  // Loop through each property and check if it matches
  for (const key of keys) {
    if (Buffer.compare(tx[key], decodedTransaction[key]) != 0) {
      throw new Error(
        `${key} mismatch: expected ${tx[key].toHex()}, got ${decodedTransaction[
          key
        ].toHex()}`
      );
    }
  }
}
