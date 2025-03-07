import { createMPT } from "@ethereumjs/mpt";
import * as RLP from "rlp";
import { keccak256 } from "ethereum-cryptography/keccak";
import { HexString } from "../types/Data";
import { Account, WorldState } from "../types/State";

/**
 * Helper function to create a storage trie root from the Account's storage.
 */
async function buildStorageRoot(storage: {
  [key: HexString]: Buffer;
}): Promise<Buffer> {
  const storageTrie = await createMPT({ useKeyHashing: true });
  for (const slot in storage) {
    const key = Buffer.from(slot.replace(/^0x/, ""), "hex").toWord();
    const value = Buffer.from(RLP.encode(storage[slot]));
    await storageTrie.put(key, value);
  }
  return Buffer.from(storageTrie.root());
}

/**
 * Encodes an Account into Ethereum-like RLP:
 * [ nonce, balance, storageRoot, codeHash ]
 */
async function encodeAccount(account: Account): Promise<Buffer> {
  // Build the storage root
  const storageRoot = await buildStorageRoot(account.storage);
  const codeHash = keccak256(account.code);

  // RLP encoding of scalar values MUST NOT have
  // leading zero. (199)
  // Positive integers must be represented in big-endian binary form with no
  // leading zeroes (thus making the integer value zero equivalent to the empty byte array).
  // See: https://github.com/ethereum/execution-specs/blob/4c7eaa840c421a1db2c01617532f31d08dc3dc6e/tests/test_rlp.py#L73

  return Buffer.from(
    RLP.encode([
      account.nonce.stripLeadingZeros(),
      account.balance.stripLeadingZeros(),
      storageRoot,
      codeHash,
    ])
  );
}

/**
 * Builds the state trie from the worldState object and returns the trie root.
 */
export async function buildStateTrieRoot(
  worldState: WorldState
): Promise<Buffer> {
  const stateTrie = await createMPT({ useKeyHashing: true });
  for (const address in worldState) {
    const key = Buffer.from(address.replace(/^0x/, ""), "hex").toAddress();
    const encodedAccount = await encodeAccount(worldState[address]);
    await stateTrie.put(key, encodedAccount);
  }
  return Buffer.from(stateTrie.root());
}
