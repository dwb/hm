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

interface EmacsFrame {
  id: string;
  title: string;
  subtitle?: string | null;
  icon?: string | null;
}

export default function Command() {
  const { data, isLoading, revalidate } = usePromise(
    () => dispatch<EmacsFrame[]>("list-frames"),
    [],
    {
      onError: showError,
    },
  );

  async function select(frame: EmacsFrame) {
    try {
      await dispatch("select-frame", { id: frame.id });
      await closeMainWindow({ popToRootType: PopToRootType.Immediate });
    } catch (error) {
      await showError(error);
    }
  }

  return (
    <List isLoading={isLoading}>
      {(data ?? []).map((frame) => (
        <List.Item
          key={frame.id}
          icon={iconFor(frame.icon, Icon.AppWindow)}
          title={frame.title}
          subtitle={frame.subtitle ?? undefined}
          actions={
            <ActionPanel>
              <Action
                title="Select Frame"
                icon={Icon.ArrowRight}
                onAction={() => select(frame)}
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
