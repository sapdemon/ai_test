# SAP Fiori (SAPUI5) On-Premise Style App: Requests Registry

This project is a simple SAPUI5 (Fiori-style) master-detail application for managing Requests and their Positions.

- Master (registry): List of Request headers with fields:
  - Created date (auto)
  - Request number (auto incremental)
  - Description (manual)
- Detail: Positions of a selected Request:
  - Position number (auto)
  - Amount (manual)
  - Currency (default UAH, editable)
  - Description
  - Department number (from directory)
- Features on detail:
  - Add position (dialog)
  - Edit position (inline)
  - Delete position (per-row delete)

The app uses JSON models and a small RequestService to manage auto-numbering and CRUD operations. It can be later wired to an OData service for on-premise deployment.

## Quick start

You can serve the `webapp/` folder via any static server. Two simple options:

- Python:

```bash
cd webapp
python3 -m http.server 8080
# open http://localhost:8080/index.html
```

- Node (http-server):

```bash
npm i -g http-server
http-server ./webapp -p 8080 -a 0.0.0.0 -c-1
# open http://localhost:8080/index.html
```

The app bootstraps SAPUI5 from CDN (`https://ui5.sap.com`).

## Structure

- `webapp/`
  - `index.html` — bootstrap and Component container
  - `Component.js` — UIComponent initialization and models
  - `manifest.json` — app descriptor and routing
  - `view/`, `controller/` — MVC views and controllers (`App`, `Master`, `Detail`)
  - `fragment/` — Dialogs (`PositionDialog`, `DepartmentValueHelp`)
  - `model/` — `models.js`, `RequestService.js`, and sample data (`Requests.json`, `Departments.json`)
  - `util/` — `formatter.js`
  - `i18n/` — UI texts
  - `css/` — custom styles

## Git

To push to your remote repository:

```bash
git init
git add .
git commit -m "Initial commit: SAPUI5 Requests app"
# then set your remote and push
# git remote add origin <YOUR_REMOTE_URL>
# git branch -M main
# git push -u origin main
```

## Notes

- Auto-numbering for requests and positions is handled in `model/RequestService.js`.
- Departments directory is loaded from `model/Departments.json` and used in a value-help dialog.
- This is a standalone UI-only implementation. For on-premise, connect to an OData service by replacing the JSON model with an `ODataModel` and mapping service operations in `RequestService`.