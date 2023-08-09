# svgrok - _to grok svg_

[Click here to see it in action!](https://bloblblobl.github.io/svgrok/public/index.html)

Svgrok is both an SVG path editor and a learning tool for SVG paths (currently
more of the former and not much of the latter). It's a work in progress, but the
goal is to make it easy to learn and understand SVG paths (and eventually other
SVG elements) by providing a visual editor and explanations of the commands.

### How To Use It

- Type in a [path definition](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/path) in the big input element at the bottom.
- Click and drag the points of the path in the editor to edit the path (and path definition)
- Shift + click on points to select multiple points at once, or shift + click on the canvas and then drag to start a selection
- Use `Q`/`W` to zoom out/zoom in
- Use the arrow keys to pan around the canvas
- Use the buttons or `Z`/`Shift + Z` to undo/redo (while your focus is not on the input)

### Building Locally

Use `elm reactor` in the root of the repo to use the built-in Elm dev server, or
run `elm make` to build an HTML file with all the JS inlined.

### Roadmap

**V0.5 (First Release)**

- [x] Improve controls/interaction
  - Single selection by default, hold shift to multi-select
  - Only initiate dragging when mouse is on a draggable element
  - Show ghost of changes while dragging
- [x] Basic Undo/Redo (store slices of state)
- [ ] Grid **(WIP)**
  - Display grid lines
  - Toggle grid on/off
  - UI to change grid increment/size
  - Auto-snap points to grid while enabled
- [ ] Drawing mode/draw tool **(WIP)**
  - Press key corresponding to command letter to enter drawing mode
  - Draw points for command step-by-step
- [ ] Clean up UI
  - Remove debug info or clean it up and move it somewhere else
  - Improve color scheme
  - Put all UI controls in a collapsible sidebar

**Future - Short Term (Probably)**

- The Final RefactorTM
  - Nest Command type under Segment
  - Save offset info in command format
  - Improve Separators
- Better Undo/Redo (store diffs of state)
- Tests
- Import/Export
- Command explanations
- Random Path Gen
- Curve Molding

**Future - Long Term (Maybe?/Hopefully)**

- Path drawing visualizer, step-by-step explanations
- WTF ("What's this for?") tool
- Realtime Collab, CRDTs
- Expand past paths - patterns, other simpler SVG shapes, animations

**Smaller Tweaks**

- Remove letter during expansion if it can be an implicit sequence
  - Make it an option?
