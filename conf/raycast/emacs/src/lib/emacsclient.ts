import { execFile } from "node:child_process";
import { homedir } from "node:os";
import path from "node:path";
import { promisify } from "node:util";
import { getPreferenceValues } from "@raycast/api";

const execFileAsync = promisify(execFile);

interface Preferences {
  emacsclientPath: string;
}

/**
 * Error raised when a call to Emacs fails. `serverDown` is set when the failure
 * looks like "no Emacs server to connect to" rather than an error inside elisp.
 */
export class EmacsError extends Error {
  readonly serverDown: boolean;

  constructor(message: string, serverDown = false) {
    super(message);
    this.name = "EmacsError";
    this.serverDown = serverDown;
  }
}

function expandTilde(p: string): string {
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return path.join(homedir(), p.slice(2));
  return p;
}

function emacsclientPath(): string {
  const pref = getPreferenceValues<Preferences>().emacsclientPath?.trim();
  return expandTilde(
    pref && pref.length > 0 ? pref : "~/.nix-profile/bin/emacsclient",
  );
}

interface Envelope<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

/**
 * Call a method on the elisp side (`raycast-emacs-dispatch`) and return its
 * decoded result.
 *
 * The wire format is deliberately escaping-free: arguments are sent as base64
 * JSON, and elisp replies with base64 JSON as well. emacsclient prints an elisp
 * string with surrounding quotes but never escapes base64 characters, so
 * decoding is just: strip quotes, base64-decode, JSON.parse.
 */
export async function dispatch<T>(
  method: string,
  args: Record<string, unknown> = {},
): Promise<T> {
  const argsB64 = Buffer.from(JSON.stringify(args), "utf8").toString("base64");
  const form = `(raycast-emacs-dispatch "${method}" "${argsB64}")`;
  const bin = emacsclientPath();

  let stdout: string;
  try {
    ({ stdout } = await execFileAsync(bin, ["--eval", form], {
      maxBuffer: 64 * 1024 * 1024,
    }));
  } catch (err) {
    throw toEmacsError(err, bin);
  }

  const envelope = decodeEnvelope<T>(stdout);
  if (!envelope.ok) {
    throw new EmacsError(envelope.error ?? "Unknown Emacs error");
  }
  return envelope.value as T;
}

function toEmacsError(err: unknown, bin: string): EmacsError {
  const e = err as { code?: string; stderr?: string; message?: string };
  if (e.code === "ENOENT") {
    return new EmacsError(
      `emacsclient not found at ${bin}. Set the path in extension preferences.`,
      true,
    );
  }
  const detail = (e.stderr ?? e.message ?? String(err)).trim();
  const serverDown =
    /can't find socket|no socket|server is not running|connection refused/i.test(
      detail,
    );
  return new EmacsError(
    serverDown
      ? "Emacs server is not running. Start it with M-x server-start."
      : detail || "emacsclient failed.",
    serverDown,
  );
}

function decodeEnvelope<T>(stdout: string): Envelope<T> {
  const unquoted = stdout.trim().replace(/^"/, "").replace(/"$/, "");
  const json = Buffer.from(unquoted, "base64").toString("utf8");
  return JSON.parse(json) as Envelope<T>;
}
