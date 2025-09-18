import {
  Action,
  ActionPanel,
  closeMainWindow,
  Icon,
  List,
  PopToRootType,
} from "@raycast/api";
import { usePromise } from "@raycast/utils";
import { dispatch } from "./lib/emacsclient";
import { iconFor, showError } from "./lib/ui";

interface EmacsBuffer {
  name: string;
  subtitle?: string | null;
  icon?: string | null;
  modified?: boolean;
}

export default function Command() {
  const { data, isLoading, revalidate } = usePromise(
    () => dispatch<EmacsBuffer[]>("list-buffers"),
    [],
    {
      onError: showError,
    },
  );

  async function switchTo(buffer: EmacsBuffer) {
    try {
      await dispatch("switch-buffer", { name: buffer.name });
      await closeMainWindow({ popToRootType: PopToRootType.Immediate });
    } catch (error) {
      await showError(error);
    }
  }

  return (
    <List isLoading={isLoading}>
      {(data ?? []).map((buffer) => (
        <List.Item
          key={buffer.name}
          icon={iconFor(buffer.icon, Icon.Document)}
          title={buffer.name}
          subtitle={buffer.subtitle ?? undefined}
          accessories={
            buffer.modified
              ? [{ icon: Icon.Dot, tooltip: "Modified" }]
              : undefined
          }
          actions={
            <ActionPanel>
              <Action
                title="Switch to Buffer"
                icon={Icon.ArrowRight}
                onAction={() => switchTo(buffer)}
              />
              <Action
                title="Reload"
                icon={Icon.ArrowClockwise}
                shortcut={{ modifiers: ["cmd"], key: "r" }}
                onAction={revalidate}
              />
            </ActionPanel>
          }
        />
      ))}
    </List>
  );
}
