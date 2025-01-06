export type HexString = `0x${string}`;

declare global {
  interface Buffer {
    /**
     * Converts a Buffer to a hexadecimal string.
     *
     * @returns The hexadecimal string representation of the input Buffer.
     */
    toHex(): HexString;

    /**
     * Converts a Buffer to a BigInt.
     *
     * @returns The BigInt representation of the input Buffer.
     */
    toBigInt(): bigint;

    /**
     * Converts a Buffer to an address Buffer.
     *
     * @returns The address Buffer representation of the input Buffer.
     */
    toAddress(): Buffer;

    /**
     * Converts a Buffer to a word Buffer.
     *
     * @returns The word Buffer representation of the input Buffer.
     */
    toWord(): Buffer;

    /**
     * Trims leading zeros from a `Buffer`.
     * @return {Buffer}
     *
     */
    stripLeadingZeros(): Buffer;
  }
}

Buffer.prototype.toHex = function (): HexString {
  return `0x${this.toString("hex")}`;
};

Buffer.prototype.toBigInt = function (): bigint {
  return BigInt(`0x${this.toString("hex")}`);
};

Buffer.prototype.toAddress = function (): Buffer {
  return this._toFixedLengthBuffer(20);
};

Buffer.prototype.toWord = function (): Buffer {
  return this._toFixedLengthBuffer(32);
};

Buffer.prototype._toFixedLengthBuffer = function (length: number): Buffer {
  if (this.length >= length) {
    return this.slice(-length);
  } else {
    const padding = Buffer.alloc(length - this.length, 0);
    return Buffer.concat([padding, this]);
  }
};

/**
 * Trims leading zeros from a `Buffer`.
 * @return {Buffer}
 */
Buffer.prototype.stripLeadingZeros = function (): Buffer {
  let a = this;
  let first = a[0];
  while (a.length > 0 && first === 0) {
    a = a.subarray(1);
    first = a[0];
  }
  return a;
};

/**
 * Converts a hexadecimal string to a Buffer.
 *
 * @param hex - The hexadecimal string to be converted.
 * @returns The Buffer representation of the input hexadecimal string.
 */
export const intToHex = (int: number): HexString => {
  return `0x${int.toString(16)}`;
};

const padToEven = (value: string): string => {
  if (value.length % 2) {
    console.log(`${value} -> 0${value}`);
    return `0${value}`;
  }
  return value;
};

/**
 * Converts a hexadecimal string to a Buffer.
 *
 * @param hex - The hexadecimal string to be converted.
 * @returns The Buffer representation of the input hexadecimal string.
 */
export const hexToBuffer = (hex: string): Buffer => {
  if (hex.startsWith("0x")) {
    hex = hex.slice(2);
  }
  return Buffer.from(padToEven(hex), "hex");
};

/**
 * Parses an object and converts any hexadecimal string values that start with "0x" to Buffers.
 *
 * @param obj - The object to be parsed. It can be of any type.
 * @returns The parsed object with hexadecimal strings converted to Buffers. If the input is not an object or is null, it returns the input as is.
 *
 * @remarks
 * - If the input is an array, each item in the array is processed recursively.
 * - If the input is an object, each property value is checked. If the value is a string that starts with "0x", it is converted to a Buffer. Otherwise, the value is processed recursively.
 */
export function parseHexToBytes(obj: any): any {
  if (typeof obj === "string" && obj.startsWith("0x")) {
    // If obj is a string that starts with "0x", convert it to a Buffer
    return hexToBuffer(obj);
  }

  if (typeof obj !== "object" || obj === null) {
    // If obj is not an object or is null, return as is
    return obj;
  }

  if (Array.isArray(obj)) {
    // If obj is an array, process each item
    return obj.map((item) => parseHexToBytes(item));
  }

  // If obj is an object, iterate over its properties
  const result: { [key: string]: any } = {};

  for (const [key, value] of Object.entries(obj)) {
    if (typeof value === "string" && value.startsWith("0x")) {
      result[key] = hexToBuffer(value);
    } else {
      // Otherwise, process the value recursively
      result[key] = parseHexToBytes(value);
    }
  }

  return result;
}
