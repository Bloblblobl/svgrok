# svgrok
to grok svg

## Roadmap
V1
 - [ ] Grid
    - Display grid lines
    - Toggle grid on/off
    - UI to change grid increment/size
    - Auto-snap points to grid while enabled
 - [ ] Improve controls/interaction
   - Single selection by default, hold shift to multi-select
   - Only initiate dragging when mouse is on a draggable element
   - Show ghost of changes while dragging
 - [ ] Drawing mode/draw tool
    - Press key corresponding to command letter to enter drawing mode
    - Draw points for command step-by-step
 - [ ] Clean up UI
    - Remove debug info or clean it up and move it somewhere else
    - Improve color scheme
    - Put all UI controls in a collapsible sidebar

Future - Short Term (Probably)
 - The Final RefactorTM
    - Nest Command type under Segment
    - Save offset info in command format
    - Improve Separators
 - Tests
 - Random Path Gen
 - Curve Molding
 - Import/Export

Future - Long Term (Maybe?/Hopefully)
 - Realtime Collab, CRDTs
 - Expand past paths - patterns, other simpler SVG shapes, animations
