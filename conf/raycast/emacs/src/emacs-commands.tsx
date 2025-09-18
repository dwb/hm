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

interface EmacsCommand {
  id: string;
  title: string;
  subtitle?: string | null;
  icon?: string | null;
}

export default function Command() {
  const { data, isLoading, revalidate } = usePromise(
    () => dispatch<EmacsCommand[]>("list-commands"),
    [],
    {
      onError: showError,
    },
  );

  async function run(command: EmacsCommand) {
    try {
      await dispatch("run-command", { id: command.id });
      await closeMainWindow({ popToRootType: PopToRootType.Immediate });
    } catch (error) {
      await showError(error);
    }
  }

  return (
    <List isLoading={isLoading}>
      {(data ?? []).map((command) => (
        <List.Item
          key={command.id}
          icon={iconFor(command.icon, Icon.Terminal)}
          title={command.title}
          subtitle={command.subtitle ?? undefined}
          actions={
            <ActionPanel>
              <Action
                title="Run in Emacs"
                icon={Icon.Play}
                onAction={() => run(command)}
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
