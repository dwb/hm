export def --wrapped main [...rest: string] {
  cd ~/Dump/ArchiveBox
  docker compose run --rm --remove-orphans archivebox ...$rest
}
