import { HexString } from "./Data";

export interface PostState {
  hash: Buffer;
  indexes: {
    data: number;
    gas: number;
    value: number;
  };
  logs: Buffer;
  txbytes: Buffer;
}

interface Account {
  nonce: Buffer;
  balance: Buffer;
  storage: { [key: HexString]: Buffer };
  code: Buffer;
}

export interface WorldState {
  [key: HexString]: Account;
}

export interface Transaction {
  nonce: Buffer;
  gasPrice: Buffer;
  gasLimit: Buffer;
  data: Buffer;
  value: Buffer;
  to: Buffer;
  v?: Buffer;
  r?: Buffer;
  s?: Buffer;
}

export interface Fixture {
  pre: WorldState;
  post: {
    [key: string]: PostState[];
  };
  transaction: {
    data: Buffer[];
    gasLimit: Buffer[];
    gasPrice: Buffer;
    nonce: Buffer;
    secretKey: Buffer;
    sender: Buffer;
    to: Buffer;
    value: Buffer[];
  };
}
