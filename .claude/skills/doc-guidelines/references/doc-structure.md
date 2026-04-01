# SmartSetup Documentation Structure

## File Locations

### Folder Layout (all products follow this)

```
source/
├── toc.yml                # Root TOC: lists top-level sections (guide, about)
├── index.md               # Homepage (uses template variables)
├── docfx.json             # DocFX build config
├── guide/                 # Conceptual and how-to content
│   ├── toc.yml            # Navigation for the guide section
│   ├── index.md           # Guide landing page
│   └── <topic-name>.md    # Individual topic files
├── reference/             # Command reference content (API, CLI, etc.)
│   ├── toc.yml            # Navigation for the reference section
│   ├── index.md           # Guide landing page - list of commands
│   └── <tms-command>.md   # Individual command files
└── images/                # PNG, SVG, ICO files
```

## Topic File Format

Every topic file starts with YAML frontmatter containing a `uid`:

```markdown
---
uid: SmartSetup.TopicName
---

# Topic Title

Content here...
```

### UID Naming Convention

- Conceptual topics: `SmartSetup.FeatureName` or `SmartSetup.Subject`.
- Command topics: `SmartSetup.Command.Build` or `SmartSetup.Command.ListRemote`.
- Shared/common sections: simple names (e.g., `Start`, `ReleaseNotes`, `Copyright`, `BreakingChanges`)
- Use PascalCase for multi-word names (e.g., `SmartSetup.Configuration`, `SmartSetup.ContinuousIntegration`).

## Table of Contents Files

### Root TOC (toc.yml)

Typical content:

```yaml
- name: Documentation
  href: guide/
- name: Reference
  href: reference/
- name: Download
  href: download/
```

### Section TOC (\_toc.md)

Each section folder has a `_toc.md` listing its pages using xref links:

```markdown
# [Getting Started](xref:Start)

Getting started with SmartSetup.

# [Feature Name](xref:ProductName.FeatureName)

Description of the feature.

# [Another Feature](xref:ProductName.AnotherFeature)

Description of another feature.
```

Each `#` heading becomes a navigation entry. The xref target must match the `uid` in the linked file.

## Adding a New Topic

1. Create the `.md` file in the appropriate folder (usually `guide/`).
2. Add YAML frontmatter with a unique `uid` following the naming convention.
3. Add an entry in the section's `_toc.md` and `toc.yml` files at the appropriate position.
4. Use `#` for the main title, `##` for major sections.

## Adding to Release Notes

The release notes file uses this format for each version:

```markdown
## Version X.Y (Month, Year)

- **New:** Description of new feature.
- **Improved:** Description of improvement.
- **Fixed:** Description of bug fix.
```

Add new items under the current version heading. Each item starts with a bold category tag. The "New" section also should have bolded text in the initial description of the feature in the release notes file.
