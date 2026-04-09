# XMLDocer Documentation Guidelines

## Markdown Structure

- `#` Main title (one per file), `##` Major section, `###` Subsection, `####` sparingly.
- Keep sections short and focused. Prefer many small sections over large blocks.
- Use `-` for unordered lists, `1.` for ordered steps.
- **Bold** for key terms/warnings, _Italic_ for variables/abstract concepts.
- No emojis, no informal language, no marketing tone.

## Code Blocks

Always use fenced blocks with the proper language tag (`delphi` if the code is in Pascal/Delphi, `shell` for CLI commands, etc.):

```delphi
uses
  System.Classes;
```

```shell
tms build -full
```

Examples must be accurate and correspond to the actual commands and parameters. Do not include placeholder text or pseudo-code. If an example is not available, it's better to omit it than to provide an inaccurate one.

## Custom Link Syntax (MANDATORY)

### Cross-Reference to Another Topic

```markdown
[Topic Title](xref:TopicUID)
[Section in Topic](xref:TopicUID#section-heading)
```

Section anchors: lowercase, spaces replaced by dashes.

### Alert Blocks

```
{{#Note}}
Content here with regular Markdown.
{{/Note}}

{{#Warning}}
Warning content here.
{{/Warning}}
```

### Image References

```
{{#image}}filename.png{{/image}}
```

Images are stored in the `images/` folder under the doc source root.

## Content Philosophy

- In guides, practical over theoretical. Show real usage with working examples.
- Explain behavior. Answer "How do I use this?" and "What happens internally?"
- Do not repeat documentation already covered in the reference. Link to it instead.
- Short paragraphs, no filler text, no speculation.
- When adding info, check if it fits in an existing topic first. Only create new topics for major concepts.

## Writing Style

- Professional, neutral, clear, direct tone.
- Avoid version-sensitive statements unless needed.
- No hard-coded assumptions. Easy to update and maintain.
