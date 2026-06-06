import sys
import re

with open(r'c:\Users\vykru\source\repos\Hywe\src\Hywe.Web\IN-ModelHelpers.fs', 'r', encoding='utf-8') as f:
    content = f.read()

# Replace simple inline if-else with match
content = re.sub(r'if (\w+) then ("[^"]+") else ("[^"]+")', r'match \1 with true -> \2 | false -> \3', content)
content = re.sub(r'if (\w+) then (Some \w+) else None', r'match \1 with true -> \2 | false -> None', content)

# Specific replaces:
content = content.replace('if model.InstallPromptAvailable then', 'match model.InstallPromptAvailable with\\n            | false -> empty()\\n            | true ->')

content = content.replace('if isWorkspaceCollapsed then "display: none;" else', 'match isWorkspaceCollapsed with true -> "display: none;" | false ->')
content = content.replace('if isWorkspaceCollapsed then "preset-drawer collapsed" else', 'match isWorkspaceCollapsed with true -> "preset-drawer collapsed" | false ->')

content = content.replace('if model.ShowLinkCopied then "Link Shared!" else', 'match model.ShowLinkCopied with true -> "Link Shared!" | false ->')
content = content.replace('if model.ShowLinkCopied then "Copied" else', 'match model.ShowLinkCopied with true -> "Copied" | false ->')
content = content.replace('if model.ShowLinkCopied then "#27ae60" else', 'match model.ShowLinkCopied with true -> "#27ae60" | false ->')

content = content.replace('if model.Tree.ActiveLevel = 0 then 0, 23\\n        else if baseSqn.StartsWith "V" then 0, 11\\n        else 12, 23', 'match model.Tree.ActiveLevel, baseSqn.StartsWith "V" with\\n        | 0, _ -> 0, 23\\n        | _, true -> 0, 11\\n        | _, false -> 12, 23')

content = content.replace('if model.ActivePanel = BoundaryPanel then "display: block;" else "display: none;"', 'match model.ActivePanel = BoundaryPanel with true -> "display: block;" | false -> "display: none;"')

content = content.replace('if lvl = 0 then "L0" else sprintf "L%d" lvl', 'match lvl with 0 -> "L0" | _ -> sprintf "L%d" lvl')

content = content.replace('if hostIds.IsEmpty then\\n                    model.Derived.cxCxl1, model.Derived.cxClr1\\n                else', 'match hostIds.IsEmpty with\\n                | true ->\\n                    model.Derived.cxCxl1, model.Derived.cxClr1\\n                | false ->')

content = content.replace('if model.ViewLocked then " active" else ""', 'match model.ViewLocked with true -> " active" | false -> ""')
content = content.replace('if model.ViewLocked then "View Locked: Captured for cover" else "Lock 3D view for report cover"', 'match model.ViewLocked with true -> "View Locked: Captured for cover" | false -> "Lock 3D view for report cover"')

content = content.replace('if model.ViewLocked then\\n                            drawMenuIcon pathLock\\n                        else\\n                            drawMenuIcon pathUnlock', 'match model.ViewLocked with\\n                        | true -> drawMenuIcon pathLock\\n                        | false -> drawMenuIcon pathUnlock')

content = content.replace('if rawResults.Length > 0 && model.BatchProgress = 24 then', 'match rawResults.Length > 0 && model.BatchProgress = 24 with\\n                | true ->')

content = content.replace('else\\n                    div {', '| false ->\\n                    div {')

content = content.replace('if isComplete then "rgba(136, 136, 136, 0.4)" else "transparent"', 'match isComplete with true -> "rgba(136, 136, 136, 0.4)" | false -> "transparent"')

content = content.replace('if model.ShowSuccessMessage then', 'match model.ShowSuccessMessage with\\n                | false -> empty()\\n                | true ->')


with open(r'c:\Users\vykru\source\repos\Hywe\src\Hywe.Web\IN-ModelHelpers.fs', 'w', encoding='utf-8') as f:
    f.write(content)
