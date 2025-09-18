import { Icon, showToast, Toast } from "@raycast/api";

/**
 * Resolve a Raycast Icon by name, as supplied from elisp. Unknown names fall
 * back to `fallback` so elisp can pass any string without breaking the UI.
 */
export function iconFor(name: string | null | undefined, fallback: Icon): Icon {
  if (!name) return fallback;
  const icon = (Icon as Record<string, Icon>)[name];
  return icon ?? fallback;
}

export async function showError(error: unknown): Promise<void> {
  await showToast({
    style: Toast.Style.Failure,
    title: "Emacs",
    message: error instanceof Error ? error.message : String(error),
  });
}
