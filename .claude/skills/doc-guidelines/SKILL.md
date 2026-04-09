---
name: xmldocer
description: Create and update documentation topics in Smart Setup documentation. Use when the user asks to write, update, add, or modify documentation topics in Smart Setup documentation. This includes writing new documentation topics, updating existing topics with new features, adding sections to existing pages, creating examples, and writing release notes entries. Triggers on requests like "document this feature", "add documentation", "update the docs", "write a new topic about Y", "add to release notes".
---

# Smart Setup Documentation Writer

Write and update Markdown documentation using proper Markdown syntax.

## Before Writing

1. Read [references/doc-guidelines.md](references/doc-guidelines.md) for formatting rules and custom syntax.
2. Read [references/doc-structure.md](references/doc-structure.md) for file locations, UID conventions, and TOC management.

## Workflow

### Updating an Existing Topic

1. Read the existing topic file.
2. Find the right section to add or modify content.
3. Follow the existing style and structure of the file.

### Creating a New Topic

1. Read 1-2 existing topic files from the same product to match style.
2. If relevant read the product source code related to the feature to ensure accuracy.
3. Create the file in the proper` folder with proper frontmatter:

```markdown
---
uid: SmartSetup.TopicName
---

# Topic Title

Content...
```

4. Add the topic to `<folder>/toc.yaml` at the appropriate position:

```markdown
- name: <Topic Title>
  href: <file-name>.md
```

### Adding Release Notes

Add items to the what's new file under the current version heading, only if user asked to update the release notes.

```markdown
- **New:** Description of the feature, the main phrase describing the change should be in bold.
- **Improved:** Description of the improvement.
- **Fixed:** Description of the fix.
```

## Key Rules

- Always use `[text](xref:UID)` for cross-topic links, never raw file paths.
- Use `{{#Note}}...{{/Note}}` and `{{#Warning}}...{{/Warning}}` for alerts.
- Use `{{#image}}filename.png{{/image}}` for images.
- Do not invent commands or parametes. Reference the real tool.
- Practical examples over theory. Show working workflows.
- In reference, focus on the technica details, parameters, and usage of the command/tool. In conceptual, focus on the why, when, and how to use the feature.
- Do not duplicate information in conceptual and reference topics. From conceptual, link to the reference for technical details. From reference, link to the conceptual for explanations and examples.
- Professional tone, no emojis, no filler.
