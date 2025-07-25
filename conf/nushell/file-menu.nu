export-env {
  $env.config.menus = ($env.config.menus | where name != my_tree_file_menu | append {
    name: my_tree_file_menu
    only_buffer_difference: false
    marker: "* "
    type: {
        layout: description
        page_size: 50
    }
    style: {
        text: green
        selected_text: green_reverse
        description_text: yellow
    }
    source: { |buffer, position|
      let origq = $buffer | str substring 0..$position | parse -r `(?<q>\S+)\z` | get 0.q
      let start = $buffer | str index-of -e $origq
      let q = $"\(?i)($origq | str replace --all '/' ".*/.*")"
      let dir = (pwd)
      glob --exclude [.* .*/**] **/* |
        each { path relative-to $dir } |
        where { $in =~ $q } | 
        each {
          {
            value: $in
            span: {
              start: $start
              end: ($start + ($in | str length))
            }
          }
        }
    }
  })

  $env.config.keybindings = ($env.config.keybindings | where name != my_tree_file_menu | append {
    name: my_tree_file_menu
    modifier: control

    keycode: char_f
    mode: [emacs vi_insert vi_normal]
    event: {
      until: [
        { send: Menu name: my_tree_file_menu }
        { send: MenuPageNext }
      ]
    }
  })
}
